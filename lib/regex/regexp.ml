type 'a regex =
  | Letter of 'a
  | Concat of 'a regex * 'a regex
  | Union of 'a regex * 'a regex
  | Star of 'a regex
  | Plus of 'a regex
  | Option of 'a regex
  | NTimes of 'a regex * int
