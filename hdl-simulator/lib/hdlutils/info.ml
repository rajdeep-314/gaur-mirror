type ('d, 'i) t =
  { data : 'd
  ; info : 'i
  }

let make d i =
  { data=d
  ; info=i
  }
