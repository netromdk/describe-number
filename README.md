# discover-point
Discover information about value at point or region in Emacs. If value is a number then binary/octal/decimal/hexadecimal/character values and conversions are shown. For strings each character is processed in the same way.

Use `discover-at-point` on point/region or `discover-value` to input value manually. Might be preferable to bind `discover-at-point` to some key.

# Demo
When using "101" as input to `discover-at-point` or `discover-value`:
![](demo-num.png)

In the case of using a string, e.g. "bin", it will process each character separately:
![](demo-string.png)

"b->d" means binary to decimal, "o->d" means octal to decimal, and "x->d" means hexadecimal to decimal.
