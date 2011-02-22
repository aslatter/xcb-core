

void xcb_ffi_return_socket(void *closure)
{
  int *has_socket;
  has_socket = (int*) closure;
  if(has_socket) {
    *has_socket = 0;
  }
}
