vegetables :: Tree String Int
vegetables = Node "pickle" 5
                  (Node "avocado" 2
                        Leaf
                        (Node "clementine" 10
                              Leaf
                              Leaf))
                  (Node "tomato" 7
                        (Node "radish" 9
                              Leaf
                              Leaf)
                        (Node "yam" 1
                              Leaf
                              Leaf))

languages :: Tree String Char
languages = Node "Haskell" 'x'
                 (Node "C" 'q'
                       (Node "Ada" 'p'
                             Leaf
                             Leaf)
                       (Node "C++" 'r'
                             Leaf
                             (Node "F#" 'e'
                                   Leaf
                                   Leaf)))
                 (Node "OCaml" 'd'
                       Leaf
                       Leaf)
