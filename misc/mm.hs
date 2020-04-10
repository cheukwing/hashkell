mmult a b
  = mmult1 a (transpose b);

mmult1 a b
  = if null a
      then []
      else mmult2 (head a) b : mmult1 (tail a) b;

mmult2 a b
  = if null b
      then []
      else mmult3 a (head b) : mmult2 a (tail b);

mmult3 a b
  = if null a || null b
      then 0
      else head a * head b + mmult3 (tail a) (tail b);
