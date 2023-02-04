# d24n-unstatic

This is a reproduction of the current [d24n.org](https://www.d24n.org/) website
built with the static-site-generator generator
[unstatic](https://github.com/swaldman/unstatic).

---



## Limitations

* The current RSS feed link — https://d24n.org/feed/ —
  takes the form of a directory, and we cannot simulate
  this by generating into `index.html` and letting a
  default configured webserver handle it. We generate
  into `/feed/index.rss`, but will have to configure
  the webserver to treat this as a directory index.

* The current site implements and links to archives by
  author. We can pretty trivially add these, but I don't
  know that we care to. In theory there might be links
  out there we might break by not adding these, but I
  doubt that there are in fact.