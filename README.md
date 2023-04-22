# store
Utility procedure for storing value to optional variable.

```Fortran
subroutine do_something(..., stat)
    implicit none
    :
    :
    integer(int32), intent(out), optional :: stat

    ! want to write
    call store(stat, 0)
    ! instead of
    if (present(stat)) then
        stat = 0
    end if
end subroutine do_something
```

### Get the code
To get the code, execute the following commnad:

```console
git clone https://github.com/degawa/store.git
cd store
```

### Reference from your project
Add the following `use` statement to modules or procedures calling store.

```Fortran
use :: store_proc
```

### Reference as a fpm project's dependency
To use store in your fpm project, add the following to the fpm.toml.

```TOML
[dependencies]
store = {git = "https://github.com/degawa/store.git"}
```

## Todo
- [ ] To write README.
- [ ] To add tests.