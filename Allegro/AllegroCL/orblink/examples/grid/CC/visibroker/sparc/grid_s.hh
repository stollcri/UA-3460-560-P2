#ifndef _grid_s_hh
#define _grid_s_hh

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

#include "grid_c.hh"


class _sk_example {
public:

  class  _sk_grid : public  example::grid {
  protected:

    _sk_grid(const char *_obj_name = (const char *)NULL);
    _sk_grid(
        const char *_service_name,
        const CORBA::ReferenceData& _data);
    virtual ~_sk_grid() {}

  public:
    static const CORBA::TypeInfo _skel_info;
    
    // No op function to force base skeletons to be linked in
    static void ___noop();
    // The following operations need to be implemented

    virtual CORBA::Short width() = 0;
    virtual void set(
        CORBA::Short n,
        CORBA::Short m,
        const char* value) = 0;
    virtual CORBA::Short height() = 0;
    virtual char* get(
        CORBA::Short n,
        CORBA::Short m) = 0;
    
    // Skeleton Operations implemented automatically

    static void _get_width(
        void *_obj,
        CORBA::MarshalInBuffer &_istrm,
        CORBA::Principal_ptr _principal,
        const char *_oper,
        void *_priv_data);

    static void _set(
        void *_obj,
        CORBA::MarshalInBuffer &_istrm,
        CORBA::Principal_ptr _principal,
        const char *_oper,
        void *_priv_data);

    static void _get_height(
        void *_obj,
        CORBA::MarshalInBuffer &_istrm,
        CORBA::Principal_ptr _principal,
        const char *_oper,
        void *_priv_data);

    static void _get(
        void *_obj,
        CORBA::MarshalInBuffer &_istrm,
        CORBA::Principal_ptr _principal,
        const char *_oper,
        void *_priv_data);

  };

};

template <class T>
class example_tie_grid : public example::grid {
private:
  CORBA::Boolean _rel_flag;
  T& _ref;

public:
  example_tie_grid(
      T& _t,
      const char *_obj_name=(char*)NULL,
      CORBA::Boolean _r_f=0)
    :example::grid(_obj_name), _ref(_t) {
    _rel_flag = _r_f;
    _object_name(_obj_name);
  }

  example_tie_grid(
      T& _t,
      const char *_serv_name,
      const CORBA::ReferenceData& _id,
      CORBA::Boolean _r_f=0)
    :_ref(_t) {
    _rel_flag = _r_f;
    _service(_serv_name, _id);
  }

  ~example_tie_grid() { if (_rel_flag) delete &_ref; }

  CORBA::Boolean rel_flag() { return _rel_flag; }
  void rel_flag(CORBA::Boolean _r_f) { _rel_flag = _r_f; }

  
  CORBA::Short width() { return _ref.width(); }
  void set(
      CORBA::Short n,
      CORBA::Short m,
      const char* value) {
    _ref.set(
        n,
        m,
        value);
  }

  CORBA::Short height() { return _ref.height(); }
  char* get(
      CORBA::Short n,
      CORBA::Short m) {
    return _ref.get(
        n,
        m);
  }

};


#endif

