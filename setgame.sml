structure SetGame : sig

  type shape
  type color
  type number
  type filling
  type card

  val shape : card -> shape 

  val color : card -> color 

  val number : card -> number

  val filling : card -> filling

  val isSet : card * card * card -> bool

  val mkCard : shape * color * number * filling -> card

  val allCards : card list 

  val chooseRandomCard : card list -> card
    
  val shapeRank : shape -> int

  val colorRank : color -> int

  val numberRank : number -> int

  val fillingRank : filling -> int

  val cardRank : card -> int

  val findSets : card list -> (card * card * card) list

end = struct

  datatype shape = Diamond | Oval | Squiggle
  datatype color = Red | Blue | Green
  datatype number = One | Two | Three
  datatype filling = Solid | None | Stripes 

  datatype trait = 
    Shape of shape | Color of color 
    | Number of number | Filling of filling

  datatype card = Card of {
    shape: shape, color: color, 
    number: number, filling: filling
  }

  fun shape (Card {shape = s, ...}) = s 

  fun color (Card {color = c, ...}) = c 

  fun number (Card {number = n, ...}) = n 

  fun filling (Card {filling = f, ...}) = f 

  fun isSet (card1, card2, card3) = let

    val cards = [card1, card2, card3]

    val getTraitFuncs = [
      fn card => Shape (shape card), 
      fn card => Color (color card),
      fn card => Number (number card),
      fn card => Filling (filling card) 
    ]

    fun areAllOrNoneEqual (t1, t2, t3) =
      (t1 = t2 andalso t1 = t3)
      orelse (t1 <> t2 andalso t2 <> t3 andalso t3 <> t1)

  in
    foldl (fn (getTrait, boolAcc) => let
      val traits = (map getTrait cards)
      val threeTraits = (
        List.nth (traits, 0), 
        List.nth (traits, 1),
        List.nth (traits, 2)
      )
    in
      boolAcc andalso areAllOrNoneEqual threeTraits
    end) true getTraitFuncs 
  end


  fun mkCard (shape, color, number, filling) =
    Card {shape = shape, color = color, number = number, filling = filling}

  val allCards = let
    val traitTable = [
      map (fn s => Shape s) [Diamond, Oval, Squiggle],
      map (fn c => Color c) [Red, Blue, Green],
      map (fn n => Number n) [One, Two, Three],
      map (fn f => Filling f) [Solid, None, Stripes]
    ]

    fun loop(table) = case table  
      of [] => [[]]
      | traits :: tableTail =>
        let val traitMixList = loop(tableTail) in
          List.concat (map (fn trait =>
            map (fn traitMix =>

              trait :: traitMix

            ) traitMixList 
          ) traits)
        end

     val traitMixList = loop(traitTable)
  in
    map (fn traitMix => 
      mkCard (
        case List.nth (traitMix, 0) of Shape s => s,
        case List.nth (traitMix, 1) of Color c => c,
        case List.nth (traitMix, 2) of Number n => n,
        case List.nth (traitMix, 3) of Filling f => f
      )
    ) traitMixList 
  end

  val seed = Random.rand (0, 81)

  fun chooseRandomCard cards = let
    val max = (length cards) - 1
    val i = Random.randRange(0, max)(seed)
  in
    List.nth (cards, i)
  end
    
  fun shapeRank shape = case shape
    of Diamond => 54 | Oval => 27 | Squiggle => 0

  fun colorRank color = case color
    of Red => 18 | Blue => 9 | Green => 0

  fun numberRank number = case number
    of One => 6 | Two => 3 | Three => 0

  fun fillingRank filling = case filling
    of Solid => 2 | None => 1 | Stripes => 0

  fun cardRank card =
    shapeRank (shape card)
    + colorRank (color card)
    + numberRank (number card)
    + fillingRank (filling card)


  structure CardSet = SplaySetFn (struct
    type ord_key = card 
    fun compare (c1, c2) = 
      if c1 = c2 
      then EQUAL
      else if cardRank c1 < cardRank c2
      then LESS else GREATER
  end)


  structure CardSetSet = SplaySetFn (struct
    type ord_key = CardSet.set 
    val compare = CardSet.compare 
  end)


  fun findSets cards = let 
    val cardSetList = List.concat (map (fn c3 => 
      List.concat (map (fn c2 => 
        map (fn c1 => 
          if isSet (c1, c2, c3)
          then CardSet.fromList [c1, c2, c3]
          else CardSet.empty
        ) cards 
      ) cards)  
    ) cards)  
    
    val cardSetSet = CardSetSet.filter (fn cardSet => 
      CardSet.numItems cardSet = 3
    ) (CardSetSet.fromList cardSetList)
  in
    map (fn cardSet => let 
      val cs = CardSet.listItems cardSet 
    in
      (List.nth (cs, 0), List.nth (cs, 1), List.nth (cs, 2))
    end) (CardSetSet.listItems cardSetSet)
  end



  val card1 = Card {shape = Diamond, color = Red, number = One, filling = Solid}
  val card2 = Card {shape = Diamond, color = Blue, number = Two, filling = Solid}
  val card3 = Card {shape = Diamond, color = Green, number = Three, filling = Solid}

  val test1 = isSet(card1, card2, card3) = true

  val card4 = Card {shape = Diamond, color = Green, number = Two, filling = Stripes}

  val test2 = isSet(card1, card2, card4) = false

end
