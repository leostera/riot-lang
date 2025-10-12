type person = { name : string; age : int }

let have_birthday p = { p with age = p.age + 1 }
