type ('d, 'i) t =
  { data : 'd
  ; info : 'i
  }

let make d i =
  { data=d
  ; info=i
  }

let map fd fi i =
  { data=fd i.data
  ; info=fi i.info
  }

let map_data fd i = map fd (fun x -> x) i

let map_info fi i = map (fun x -> x) fi i
