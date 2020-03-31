# zdomaintextgetabap

# Task Description

For a given value, for which the variable type is based on a dictionary domain, get the text description for the value from the dictionary (value table, fixed values, range values).

# Algorithm
1. Required text is returned as a return parameter of the static method get_domain_descr. IFor input parameter, the method takes the value in a variable.
2. using rtts class methods, the underlying domain is determined.
3. presence of value table is checked. If value table is present, standard class cl_text_identifier is used to determine key relationship between domain value and value table, as well as any text table that might be associated with the value table. Also the prospective text field in the value or text table is defined at this stage.
4. Based on information in p. 3, a dynamic select query is formed and a table of values is selected and saved in a buffer.
5. Required text for specific value is read from the buffer into the returning parameter.
6. in case of fixed/range values(там всё просто).
# Points worth noting
1. Incase method is called in a loop only once, buffering works okay. However if it's called several times buffering does not work if domains are different ( NB: works even if domains are different but same value table). 
2. As a solution for p.1, one can add their method in the class that returns а table instead.     
# Advantage
- No need to know the domain name, just pass values and voila... there you have text.
# Version:
 Works for version 7.4 and above.
