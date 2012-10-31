
/**
 **************************************************************************
 *                                                                        *
 *                        -- DO NOT MODIFY --                             *
 *  This file is automatically generated by the VisiBroker IDL compiler.  *
 *  Generated code conforms to OMG's IDL-to-C++ 1.1 mapping as specified  *
 *  in OMG Document Number: 96-01-13                                      *
 *                                                                        *
 *  VisiBroker is copyrighted by Visigenic Software, Inc.                 *
 **************************************************************************
 */

#include "grid_s.hh"

static CORBA::MethodDescription __sk_example_grid_methods[] = {
  {"_get_width", &_sk_example::_sk_grid::_get_width},
  {"set", &_sk_example::_sk_grid::_set},
  {"_get_height", &_sk_example::_sk_grid::_get_height},
  {"get", &_sk_example::_sk_grid::_get}
};

const CORBA::TypeInfo _sk_example::_sk_grid::_skel_info(
  "example::grid",
  (CORBA::ULong)4,
  __sk_example_grid_methods);

_sk_example::_sk_grid::_sk_grid(const char *_obj_name) :
    example::grid(_obj_name) {
  _object_name(_obj_name);
}

_sk_example::_sk_grid::_sk_grid(
    const char *_serv_name,
    const CORBA::ReferenceData& _id) {
  _service(_serv_name, _id);
}

void _sk_example::_sk_grid::___noop() {}
void _sk_example::_sk_grid::_get_width(
    void *_obj,
    CORBA::MarshalInBuffer &_istrm,
    CORBA::Principal_ptr ,
    const char *,
    void *_priv_data)
{
  example::grid *_impl = (example::grid *)_obj;

  VISostream& _ostrm = *(VISostream *)
    (CORBA::MarshalOutBuffer*)_impl->_prepare_reply(_priv_data);
  CORBA::Short _ret = _impl->width();
  _ostrm << _ret;
}

void _sk_example::_sk_grid::_set(
    void *_obj,
    CORBA::MarshalInBuffer &_istrm,
    CORBA::Principal_ptr _principal,
    const char *_oper,
    void *_priv_data) {
  VISistream& _vistrm = _istrm;
  example::grid *_impl = (example::grid *)_obj;

  CORBA::Short n;
  CORBA::Short m;
  CORBA::String_var value;
  _vistrm >> n;
  _vistrm >> m;
  _vistrm >> value;
  _impl->set(
      n,
      m,
      value.in());
  
  VISostream& _ostrm = *(VISostream *)
    (CORBA::MarshalOutBuffer*)_impl->_prepare_reply(_priv_data);
}

void _sk_example::_sk_grid::_get_height(
    void *_obj,
    CORBA::MarshalInBuffer &_istrm,
    CORBA::Principal_ptr ,
    const char *,
    void *_priv_data)
{
  example::grid *_impl = (example::grid *)_obj;

  VISostream& _ostrm = *(VISostream *)
    (CORBA::MarshalOutBuffer*)_impl->_prepare_reply(_priv_data);
  CORBA::Short _ret = _impl->height();
  _ostrm << _ret;
}

void _sk_example::_sk_grid::_get(
    void *_obj,
    CORBA::MarshalInBuffer &_istrm,
    CORBA::Principal_ptr _principal,
    const char *_oper,
    void *_priv_data) {
  VISistream& _vistrm = _istrm;
  example::grid *_impl = (example::grid *)_obj;

  CORBA::Short n;
  CORBA::Short m;
  _vistrm >> n;
  _vistrm >> m;
  CORBA::String_var _ret = _impl->get(
      n,
      m);
  
  VISostream& _ostrm = *(VISostream *)
    (CORBA::MarshalOutBuffer*)_impl->_prepare_reply(_priv_data);
  _ostrm << _ret;
}
