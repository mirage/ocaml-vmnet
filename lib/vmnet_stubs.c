/*
 * Copyright (C) 2014 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#include <sys/types.h>
#include <sys/uio.h>
#include <dispatch/dispatch.h>
#include <vmnet/vmnet.h>

static struct custom_operations interface_ref_ops = {
  "org.openmirage.vmnet.interface_ref",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define Interface_ref_val(v) (*((interface_ref *) Data_custom_val(v)))

static value
alloc_interface_ref(interface_ref i)
{
  value v = alloc_custom(&interface_ref_ops, sizeof(interface_ref *), 0, 1);
  Interface_ref_val(v) = i;
  return v;
}

CAMLprim value
caml_init_vmnet(value v_mode)
{
  CAMLparam1(v_mode);
  CAMLlocal1(v_iface_ref);
  xpc_object_t interface_desc = xpc_dictionary_create(NULL, NULL, 0);
  xpc_dictionary_set_uint64(interface_desc, vmnet_operation_mode_key, Int_val(v_mode));
  uuid_t uuid;
  uuid_generate_random(uuid);
  xpc_dictionary_set_uuid(interface_desc, vmnet_interface_id_key, uuid);
  __block interface_ref iface = NULL;
  dispatch_queue_t if_create_q =
    dispatch_queue_create("org.openmirage.vmnet.create", DISPATCH_QUEUE_SERIAL);
  dispatch_semaphore_t iface_created = dispatch_semaphore_create(0);
  iface = vmnet_start_interface(interface_desc, if_create_q,
    ^(vmnet_return_t status, xpc_object_t interface_param) { 
      printf("iface_handler status %d param %p\n", status, interface_param);
      if (!interface_param) return;
      const char *m = xpc_dictionary_get_string(interface_param, vmnet_mac_address_key);
      uint64_t mtu = xpc_dictionary_get_uint64(interface_param, vmnet_mtu_key);
      uint64_t packet_size = xpc_dictionary_get_uint64(interface_param, vmnet_max_packet_size_key);
      printf("mtu: %lld\npacket size: %lld\n", mtu, packet_size);
      printf("mac: %02x:%02x:%02x:%02x:%02x:%02x\n", m[0], m[0], m[2], m[3], m[4], m[5]);
      dispatch_semaphore_signal(iface_created);
    });
  dispatch_semaphore_wait(iface_created, DISPATCH_TIME_FOREVER);
  dispatch_release(if_create_q);
  if (iface == NULL)
     caml_failwith("failed to initialise interface");
  v_iface_ref = alloc_interface_ref(iface);
  CAMLreturn(v_iface_ref);
}

CAMLprim value
caml_set_event_handler(value v_iref, value v_callback_name)
{
  CAMLparam2(v_iref, v_callback_name);
  interface_ref iface = Interface_ref_val(v_iref);
  value *v_cb = caml_named_value(String_val(v_callback_name));
  if (v_cb == NULL)
     caml_failwith("callback not registered");
  /* TODO: release queue. */
  /* TODO: does name need to be unique? */
  dispatch_queue_t iface_q = dispatch_queue_create("org.openmirage.vmnet.iface_q", 0);
  vmnet_interface_set_event_callback(iface, VMNET_INTERFACE_PACKETS_AVAILABLE, iface_q,
    ^(interface_event_t event_id, xpc_object_t event)
    { caml_callback(*v_cb, Val_unit); });
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_vmnet_read(value v_iref, value v_ba, value v_ba_off, value v_ba_len)
{
  CAMLparam4(v_iref, v_ba, v_ba_off, v_ba_len);
  interface_ref iface = Interface_ref_val(v_iref);
  struct iovec iov;
  iov.iov_base = Caml_ba_data_val(v_ba) + (Int_val(v_ba_off));
  iov.iov_len = Int_val(v_ba_len);
  struct vmpktdesc v;
  v.vm_pkt_size = Int_val(v_ba_len);
  v.vm_pkt_iov = &iov;
  v.vm_pkt_iovcnt = 1;
  v.vm_flags = 0; /* TODO no clue what this is */
  int pktcnt = 1;
  vmnet_return_t res = vmnet_read(iface, &v, &pktcnt);
  if (res == VMNET_SUCCESS) 
    CAMLreturn(Val_int(v.vm_pkt_size));
  else
    CAMLreturn(Val_int((-1)*res));
}

CAMLprim value
caml_vmnet_write(value v_iref, value v_ba, value v_ba_off, value v_ba_len)
{
  CAMLparam4(v_iref, v_ba, v_ba_off, v_ba_len);
  interface_ref iface = Interface_ref_val(v_iref);
  struct iovec iov;
  iov.iov_base = Caml_ba_data_val(v_ba) + (Int_val(v_ba_off));
  iov.iov_len = Int_val(v_ba_len);
  struct vmpktdesc v;
  v.vm_pkt_size = Int_val(v_ba_len);
  v.vm_pkt_iov = &iov;
  v.vm_pkt_iovcnt = 1;
  v.vm_flags = 0; /* TODO no clue what this is */
  int pktcnt = 1;
  vmnet_return_t res = vmnet_write(iface, &v, &pktcnt);
  if (res == VMNET_SUCCESS) 
    CAMLreturn(Val_int(v.vm_pkt_size));
  else
    CAMLreturn(Val_int((-1)*res));
}

