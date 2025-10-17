type loc =
  {
    loc_start : Lexing.position;
    loc_end : Lexing.position;
  }

type 'a wrapped =
  {
    txt : 'a;
    loc : loc
  }

let mk_loc loc_start loc_end = 
  {
    loc_start=loc_start;
    loc_end=loc_end
  }

let wrap_loc ~loc txt =
  {
    txt=txt;
    loc=loc
  }
