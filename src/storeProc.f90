module store_proc
    use :: store_intrinsic_int8
    use :: store_intrinsic_int16
    use :: store_intrinsic_int32
    use :: store_intrinsic_int64

    use :: store_intrinsic_complex32
    use :: store_intrinsic_complex64

    use :: store_intrinsic_real32
    use :: store_intrinsic_real64

#if defined(__GFORTRAN__) || defined(__INTEL_COMPILER) || defined(NAGFOR)
    use :: store_intrinsic_real128
    use :: store_intrinsic_complex128
#endif

    use :: store_intrinsic_logical

#if defined(RATHER_CHAR)
    use :: store_intrinsic_char
#else
    use :: store_intrinsic_str
#endif
end module store_proc
