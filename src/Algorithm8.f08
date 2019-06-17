Eingabe: a und b /= 0, in der Variable x.
Ausgabe: q und r

Anfang
   q := 0
   r := a
   d := deg(b)
   c := lc(b)
   do while deg(r) >= d
      s := lc(r) / c * c**(deg(r) - d)
      q += s
      r -= s * b
   end do
   return (q, r)
Ende
