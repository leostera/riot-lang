type person = {
  name: string;
  company: company option;
}

and company = {
  name: string;
  employees: person list;
}
