extern crate cstr_argument;
extern crate nvpair_sys as sys;
extern crate cstr_argument;
#[macro_use]
extern crate foreign_types;

use cstr_argument::CStrArgument;
use std::io;
use std::ptr;
use std::ffi;
use std::os::raw::c_int;
use foreign_types::{Opaque, ForeignTypeRef, ForeignType};
use std::mem;
use std::ffi::CStr;
use std::os::raw::{c_double, c_int, c_uint};
use std::io::{Error, ErrorKind};

#[derive(Debug, PartialEq)]
pub enum NvData {
    Bool,
    BoolV(bool),
    Byte(u8),
    Int8(i8),
    Uint8(u8),
    Int16(i16),
    Uint16(u16),
    Int32(i32),
    Uint32(u32),
    Int64(i64),
    Uint64(u64),
    Uint64Array(Vec<u64>),
    String(ffi::CString),
    NvListRef(NvList),
    NvList(NvList),
    NvListArray(Vec<NvList>),
    Double(c_double),
    
    // TODO: arrays
    // hrtime
}

impl NvData {
    pub fn nv_list(self) -> io::Result<NvList> {
        match self {
            NvData::NvList(x) => Ok(x),
            _ => Err(Error::new(ErrorKind::NotFound, "nvList not a nvList value")),
        }
    }

    pub fn uint64(self) -> io::Result<u64> {
        match self {
            NvData::Uint64(x) => Ok(x),
            _ => Err(Error::new(ErrorKind::NotFound, "uint64 not a nvList value")),
        }
    }

    pub fn nv_list_array(self) -> Option<Vec<NvList>> {
        match self {
            NvData::NvListArray(x) => Some(x),
            _ => None,
        }
    }

    pub fn string(self) -> Option<ffi::CString> {
        match self {
            NvData::String(x) => Some(x),
            _ => None,
        }
    }
}

pub trait NvEncode {
    fn insert<S: CStrArgument>(&self, S, &mut NvListRef) -> io::Result<()>;
    //fn read(NvPair &nv) -> io::Result<Self>;
}

impl NvEncode for bool {
    fn insert<S: CStrArgument>(&self, name: S, nv: &mut NvListRef) -> io::Result<()>
    {
        let name = name.into_cstr();
        let v = unsafe {
            sys::nvlist_add_boolean_value(nv.as_mut_ptr(), name.as_ref().as_ptr(),
                if *self {
                    sys::boolean::B_TRUE
                } else {
                    sys::boolean::B_FALSE
                },
            )
        };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(())
        }
    }
}

impl NvEncode for u32 {
    fn insert<S: CStrArgument>(&self, name: S, nv: &mut NvListRef) -> io::Result<()>
    {
        let name = name.into_cstr();
        let v = unsafe {
            sys::nvlist_add_uint32(nv.as_mut_ptr(), name.as_ref().as_ptr(), *self)
        };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(())
        }
    }
}

impl NvEncode for ffi::CStr {
    fn insert<S: CStrArgument>(&self, name: S, nv: &mut NvListRef) -> io::Result<()>
    {
        let name = name.into_cstr();
        let v = unsafe {
            sys::nvlist_add_string(nv.as_mut_ptr(), name.as_ref().as_ptr(), self.as_ptr())
        };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(())
        }
    }
}

impl NvEncode for NvListRef {
    fn insert<S: CStrArgument>(&self, name: S, nv: &mut NvListRef) -> io::Result<()>
    {
        let name = name.into_cstr();
        let v = unsafe {
            sys::nvlist_add_nvlist(nv.as_mut_ptr(), name.as_ref().as_ptr(), self.as_ptr() as *mut _)
        };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
            } else {
            Ok(())
        }
    }
}

pub enum NvEncoding {
    Native,
    Xdr,
}

impl NvEncoding {
    fn as_raw(&self) -> c_int {
        match self {
            &NvEncoding::Native => sys::NV_ENCODE_NATIVE,
            &NvEncoding::Xdr => sys::NV_ENCODE_XDR,
        }
    }
}

foreign_type! {
    type CType = sys::nvlist;
    fn drop = sys::nvlist_free;
    /// An `NvList`
    pub struct NvList;
    /// A borrowed `NvList`
    pub struct NvListRef;
}

impl NvList {
    /// Create a new `NvList` with no options
    pub fn new() -> io::Result<Self> {
        let mut n = ptr::null_mut();
        let v = unsafe {
            // TODO: second arg is a bitfield of NV_UNIQUE_NAME|NV_UNIQUE_NAME_TYPE
            sys::nvlist_alloc(&mut n, 0, 0)
        };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(unsafe { Self::from_ptr(n) }) 
        }
    }

    /// Create a new `NvList` with the `NV_UNIQUE_NAME` constraint
    pub fn new_unqiue_names() -> io::Result<Self> {
        let mut n = ptr::null_mut();
        let v = unsafe {
            sys::nvlist_alloc(&mut n, sys::NV_UNIQUE_NAME, 0)
        };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(unsafe { Self::from_ptr(n) }) 
        }
    }

    pub fn try_clone(&self) -> io::Result<Self> {
        let mut n = ptr::null_mut();
        let v = unsafe { sys::nvlist_dup(self.0, &mut n, 0) };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(unsafe { Self::from_ptr(n) })
        }
    }
}

impl Clone for NvList {
    fn clone(&self) -> Self {
        self.try_clone().unwrap()
    }
}

impl NvListRef {
    pub unsafe fn from_mut_ptr<'a>(v: *mut sys::nvlist) -> &'a mut Self
    {
        std::mem::transmute::<*mut sys::nvlist, &mut Self>(v)
    }

    pub unsafe fn from_ptr<'a>(v: *const sys::nvlist) -> &'a Self
    {
        std::mem::transmute::<*const sys::nvlist, &Self>(v)
    }

    pub fn as_mut_ptr(&mut self) -> *mut sys::nvlist
    {
        unsafe { std::mem::transmute::<&mut NvListRef, *mut sys::nvlist>(self) }
    }

    pub fn as_ptr(&self) -> *const sys::nvlist
    {
        unsafe { std::mem::transmute::<&NvListRef, *const sys::nvlist>(self) }
    }

    pub fn encoded_size(&self, encoding: NvEncoding) -> io::Result<usize>
    {
        let mut l = 0usize;
        let v = unsafe {
            sys::nvlist_size(self.as_ptr() as *mut _, &mut l, encoding.as_raw())
        };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(l)
        }
    }

    pub fn is_empty(&self) -> bool {
        let v = unsafe { sys::nvlist_empty(self.as_ptr() as *mut _) };
        v != sys::boolean::B_FALSE
    }

    pub fn add_boolean<S: CStrArgument>(&mut self, name: S) -> io::Result<()>
    {
        let name = name.into_cstr();
        let v = unsafe { sys::nvlist_add_boolean(self.as_mut_ptr(), name.as_ref().as_ptr()) };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(())
        }
    }

    pub fn first(&self) -> Option<&NvPair> {
        let np = unsafe { sys::nvlist_next_nvpair(self.as_ptr() as *mut _, ptr::null_mut()) };  
        if np.is_null() {
            None
        } else {
            Some(unsafe { NvPair::from_ptr(np) })
        }
    }

    pub fn iter(&self) -> NvListIter {
        NvListIter {
            parent: self,
            pos: ptr::null_mut(),
        }
    }

    pub fn exists<S: CStrArgument>(&self, name: S) -> bool {
        let name = name.into_cstr();
        let v = unsafe { sys::nvlist_exists(self.as_ptr() as *mut _, name.as_ref().as_ptr()) };
        v != sys::boolean::B_FALSE
    }

    /* 
    // not allowed because `pair` is borrowed from `self`. Need to fiddle around so that we can
    // check:
    //  - `pair` is from `self`
    //  - `pair` is the only outstanding reference to this pair (need by-value semantics)
    pub fn remove(&mut self, pair: &NvPair) -> io::Result<()>
    {
        let v = unsafe { sys::nvlist_remove_nvpair(self.as_mut_ptr(), pair.as_ptr())};
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(())
        }
    }
    */

    pub fn lookup<S: CStrArgument>(&self, name: S) -> io::Result<&NvPair>
    {
        let name = name.into_cstr();
        let mut n = ptr::null_mut();
        let v = unsafe { sys::nvlist_lookup_nvpair(self.as_ptr() as *mut _, name.as_ref().as_ptr(), &mut n) };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(unsafe { NvPair::from_ptr(n) })
        }
    }

    pub fn lookup_nv_list<S: CStrArgument>(&self, name: S) -> io::Result<NvList> {
        let name = name.into_cstr();

        let mut n = ptr::null_mut();

        let v =
            unsafe {
                sys::nvlist_lookup_nvlist(self.as_ptr(), name.as_ref().as_ptr(), &mut n)
            };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            let r = unsafe {NvList::from_ptr(n, true)};

            Ok(r)
        }
    }

    pub fn lookup_nv_list_array<S: CStrArgument>(&self, name: S) -> io::Result<Vec<NvList>> {
        let name = name.into_cstr();

        let mut n = ptr::null_mut();

        let mut len: c_uint;

        let v =
            unsafe {
                len = mem::uninitialized();
                sys::nvlist_lookup_nvlist_array(self.as_ptr(), name.as_ref().as_ptr(), &mut n, &mut len)
            };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            let r = unsafe {
                std::slice::from_raw_parts(n, len as usize)
                    .iter()
                    .map(|x| NvList::from_ptr(*x, true))
                    .collect()
            };

            Ok(r)
        }
    }

    pub fn lookup_uint64<S: CStrArgument>(&self, name: S) -> io::Result<u64> {
        let name = name.into_cstr();
        let mut n: u64;

        let v =
            unsafe {
                n = mem::uninitialized();

                sys::nvlist_lookup_uint64(self.as_ptr(), name.as_ref().as_ptr(), &mut n)
            };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(n)
        }
    }
    pub fn lookup_string<S: CStrArgument>(&self, name: S) -> io::Result<ffi::CString> {
        let name = name.into_cstr();
        let mut n;
        
        let v = unsafe {
            n = mem::uninitialized();

            sys::nvlist_lookup_string(self.as_ptr(), name.as_ref().as_ptr(), &mut n)
        };

        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            let s = unsafe { CStr::from_ptr(n).to_owned() };
            Ok(s)
        }
    }
}

    pub fn try_to_owned(&self) -> io::Result<NvList> {
        let mut n = NvList(ptr::null_mut());
        let v = unsafe { sys::nvlist_dup(self.as_ptr() as *mut _, &mut n.0, 0) };
        if v != 0 {
            Err(io::Error::from_raw_os_error(v))
        } else {
            Ok(n)
        }
    }
}

pub struct NvListIter<'a> {
    parent: &'a NvListRef,
    pos: *mut sys::nvpair, 
}

impl<'a> Iterator for NvListIter<'a> {
    type Item = &'a NvPair;

    fn next(&mut self) -> Option<Self::Item> {
        let np = unsafe { sys::nvlist_next_nvpair(self.parent.as_ptr() as *mut _, self.pos) };  
        self.pos = np;
        if np.is_null() {
            None
        } else {
            Some(unsafe { NvPair::from_ptr(np) })
        }
    }
}

pub struct NvPair(Opaque);
impl ForeignTypeRef for NvPair {
    type CType = sys::nvpair;
}

impl NvPair {
    pub fn name(&self) -> &ffi::CStr
    {
        unsafe { ffi::CStr::from_ptr(sys::nvpair_name(self.as_ptr())) }
    }

    pub fn pair_type(&self) -> sys::data_type_t::Type {
        unsafe { sys::nvpair_type(self.as_ptr()) }
    }

    pub fn value_nv_list(&self) -> io::Result<NvList> {
        let mut nvl_target = ptr::null_mut();

        unsafe {
            let code = sys::nvpair_value_nvlist(self.as_ptr(), &mut nvl_target);

            if code == 0 {
                Ok(NvList::from_ptr(nvl_target, true))
            } else {
                Err(io::Error::from_raw_os_error(code))
            }
        }
    }

    pub fn value(&self) -> io::Result<NvData> {
        let x = self.pair_type();

        match x {
            sys::data_type_t::DATA_TYPE_UNKNOWN => panic!("Unknown type, exiting"),
            sys::data_type_t::DATA_TYPE_BOOLEAN => Ok(NvData::Bool),
            // sys::data_type_t::DATA_TYPE_BYTE => {
            //     let p = ptr::null_mut();
            //     unsafe {
            //         sys::nvpair_value_byte(self.as_ptr(), p);
            //         NvData::Byte(*p)
            //     }
            // }
            // sys::data_type_t::DATA_TYPE_INT16 => {
            //     let p = ptr::null_mut();
            //     unsafe {
            //         sys::nvpair_value_int16(self.as_ptr(), p);
            //         NvData::Int16(*p)
            //     }
            // }
            // sys::data_type_t::DATA_TYPE_UINT16 => {
            //     let p = ptr::null_mut();
            //     unsafe {
            //         sys::nvpair_value_uint16(self.as_ptr(), p);
            //         NvData::Uint16(*p)
            //     }
            // }
            // sys::data_type_t::DATA_TYPE_INT32 => {
            //     let p = ptr::null_mut();
            //     unsafe {
            //         sys::nvpair_value_int32(self.as_ptr(), p);
            //         NvData::Int32(*p)
            //     }
            // }
            // sys::data_type_t::DATA_TYPE_UINT32 => {
            //     let p = ptr::null_mut();
            //     unsafe {
            //         sys::nvpair_value_uint32(self.as_ptr(), p);
            //         NvData::Uint32(*p)
            //     }
            // }
            // sys::data_type_t::DATA_TYPE_INT64 => {
            //     let p = ptr::null_mut();
            //     unsafe {
            //         sys::nvpair_value_int64(self.as_ptr(), p);
            //         NvData::Int64(*p)
            //     }
            // }
            sys::data_type_t::DATA_TYPE_UINT64 => {
                let mut x: u64;
                let code = unsafe {
                    x = mem::uninitialized();

                    sys::nvpair_value_uint64(self.as_ptr(), &mut x)
                };

                if code == 0 {
                    Ok(NvData::Uint64(x))
                } else {
                    Err(io::Error::from_raw_os_error(code))
                }
            }
            sys::data_type_t::DATA_TYPE_STRING => unsafe {
                let p = mem::uninitialized();

                let code = sys::nvpair_value_string(self.as_ptr(), p);

                if code == 0 {
                    let s = CStr::from_ptr(*p).to_owned();
                    Ok(NvData::String(s))
                } else {
                    Err(io::Error::from_raw_os_error(code))
                }
            },
            // sys::data_type_t::DATA_TYPE_BYTE_ARRAY => (),
            // sys::data_type_t::DATA_TYPE_INT16_ARRAY => (),
            // sys::data_type_t::DATA_TYPE_UINT16_ARRAY => (),
            // sys::data_type_t::DATA_TYPE_INT32_ARRAY => (),
            // sys::data_type_t::DATA_TYPE_UINT32_ARRAY => (),
            // sys::data_type_t::DATA_TYPE_INT64_ARRAY => (),
            sys::data_type_t::DATA_TYPE_UINT64_ARRAY => unsafe {
                let mut xs = mem::uninitialized();
                let len = mem::uninitialized();
                let code = sys::nvpair_value_uint64_array(self.as_ptr(), &mut xs, len);
                if code == 0 {
                    // let s = std::slice::from_raw_parts(xs, *len as usize);
                    // Ok(NvData::Uint64Array(s.to_vec()))
                    Ok(NvData::Uint64Array(vec![]))
                } else {
                    Err(io::Error::from_raw_os_error(code))
                }
            },
            // sys::data_type_t::DATA_TYPE_STRING_ARRAY => (),
            // sys::data_type_t::DATA_TYPE_HRTIME => (),
            sys::data_type_t::DATA_TYPE_NVLIST => {
                let r = self.value_nv_list()?;

                Ok(NvData::NvList(r))
            }
            sys::data_type_t::DATA_TYPE_NVLIST_ARRAY => unsafe {
                let mut xs = ptr::null_mut();
                let len = mem::uninitialized();
                let code = sys::nvpair_value_nvlist_array(self.as_ptr(), &mut xs, len);

                if code == 0 {
                    let s = std::slice::from_raw_parts(xs, *len as usize)
                        .iter()
                        .map(|x| NvList::from_ptr(*x, false))
                        .collect();

                    Ok(NvData::NvListArray(s))
                } else {
                    Err(io::Error::from_raw_os_error(code))
                }
            }
            // sys::data_type_t::DATA_TYPE_BOOLEAN_VALUE => {
            //     let b = ptr::null_mut();
            //     unsafe {
            //         let ret = sys::nvpair_value_boolean_value(self.as_ptr(), b);

            //         if ret == 0 {
            //             Ok(NvData::BoolV(*b != sys::boolean_B_FALSE))
            //         } else {
            //             Err("Unexpected return code.".into())
            //         }
            //     }
            // }
            // sys::data_type_t::DATA_TYPE_INT8 => {
            //     let p = ptr::null_mut();
            //     unsafe {
            //         sys::nvpair_value_int8(self.as_ptr(), p);
            //         NvData::Int8(*p)
            //     }
            // }
            // sys::data_type_t::DATA_TYPE_UINT8 => {
            //     let p = ptr::null_mut();
            //     unsafe {
            //         sys::nvpair_value_uint8(self.as_ptr(), p);
            //         NvData::Uint8(*p)
            //     }
            // }
            // sys::data_type_t::DATA_TYPE_BOOLEAN_ARRAY => (),
            // sys::data_type_t::DATA_TYPE_INT8_ARRAY => (),
            // sys::data_type_t::DATA_TYPE_UINT8_ARRAY => (),
            // sys::data_type_t::DATA_TYPE_DOUBLE => {
            //     let p = ptr::null_mut();
            //     unsafe {
            //         sys::nvpair_value_double(self.as_ptr(), p);
            //         NvData::Double(*p)
            //     }
            // }
            x => {
                println!("missed on: {:?}", x);
                panic!("boom")
            }
        }
    }
}