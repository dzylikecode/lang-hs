js1 =
  JObj
    [ ("name", JStr "Ranjit"),
      ("age", JNum 33),
      ("likes", JArr [JStr "guacamole", JStr "coffee", JStr "bacon"]),
      ("hates", JArr [JStr "waiting", JStr "grapefruit"]),
      ( "lunches",
        JArr
          [ JObj
              [ ("day", JStr "monday"),
                ("loc", JStr "zanzibar")
              ],
            JObj
              [ ("day", JStr "tuesday"),
                ("loc", JStr "farmers market")
              ],
            JObj
              [ ("day", JStr "wednesday"),
                ("loc", JStr "hare krishna")
              ],
            JObj
              [ ("day", JStr "thursday"),
                ("loc", JStr "faculty club")
              ],
            JObj
              [ ("day", JStr "friday"),
                ("loc", JStr "coffee cart")
              ]
          ]
      )
    ]

hs =
  ( ("name", "Ranjit"),
    ("age", 33 :: Double),
    ("likes", ["guacamole", "coffee", "bacon"]),
    ("hates", ["waiting", "grapefruit"]),
    ("lunches", lunches)
  )