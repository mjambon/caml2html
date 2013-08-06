let add_operator ~name ~level ~value =
  EXTEND
    Pcaml.expr: LEVEL $level$ [
      [ x = SELF; $name$; y = SELF ->
          <:expr< $value$ $x$ $y$ >> ]
    ];
  END

EXTEND
  Pcaml.str_item: [
    [ "OPERATOR"; name = STRING; "LEVEL"; level = STRING;
      "VALUE"; value = Pcaml.expr; "END" ->
        add_operator
          ~name:(Token.eval_string _loc name)
          ~level:(Token.eval_string _loc level)
          ~value;
        <:str_item< declare end >> ]
  ];
END



let expand _loc e =
  <:expr< 1 +
          $e$ >>
