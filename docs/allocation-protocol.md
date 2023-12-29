
# Allocation protocol

## Error handling types

#### In place
```c
if (error) {
	if (recoverable) {
	    handle_and_continue
	} else {
        call_abort_callback
    }
}
```
#### Propagate
```c
if (error) {
    perror();
    return error;
}

// can be macro
#define EC(x) if (error = (x)) {\
    perror();\
    return error;\
}
```

### Protocol 1

Default allocator asumes infinite memory.

If can't allocate call abort callback

### Protocol 2

Default allocator return an error if there is not enough memory