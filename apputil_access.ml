module Uncaught = Mysql_access_k2v.Make (struct
  let tblname = "uncaught"
  module Key1 = Apputil_error_id
  module Key2 = Mysql_access_util.String (* random *)
  module Value = struct
    type t = Apputil_error_t.error
    let of_string x = Apputil_error_j.error_of_string x
    let to_string x = Apputil_error_j.string_of_error x
  end
  module Ord = Util_time

  let create_ord k1 k2 v = v.Apputil_error_t.error_date
  let update_ord = None
end)
