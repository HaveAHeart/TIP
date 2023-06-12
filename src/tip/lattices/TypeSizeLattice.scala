package tip.lattices

/**
 * An element of the type lattice.
 */
object typeSizeElement extends Enumeration {
  val Bool_t, Char_t, BigInt_t, Int_t, Byte_t = Value
}

/**
 * The type lattice.
 */

object TypeSizeLattice extends FlatLattice[typeSizeElement.Value] with LatticeWithOps {

  import typeSizeElement._

  private val typeSizeValues: Map[Element, Int] = Map(
    Bot -> 0,
    FlatEl(Bool_t) -> 1,
    FlatEl(Char_t) -> 2,
    FlatEl(Byte_t) -> 3,
    FlatEl(Int_t) -> 4,
    FlatEl(BigInt_t) -> 5,
    Top -> 6)

  private def lookup(op: List[List[Element]], x: Element, y: Element): Element =
    op(typeSizeValues(x))(typeSizeValues(y))

  private val absPlus: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot, Bot, Bot),
      List(Bot, Bot, Bot, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bot, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bot, Int_t, BigInt_t, BigInt_t, Top),
      List(Bot, Bot, Bot, BigInt_t, BigInt_t, BigInt_t, Top),
      List(Bot, Bot, Bot, BigInt_t, BigInt_t, BigInt_t, Top),
      List(Bot, Top, Top, Top, Top, Top, Top)
    )

  private val absMinus: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot, Bot, Bot),
      List(Bot, Bot, Bot, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bot, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bot, Int_t, BigInt_t, BigInt_t, Top),
      List(Bot, Bot, Bot, BigInt_t, BigInt_t, BigInt_t, Top),
      List(Bot, Bot, Bot, BigInt_t, BigInt_t, BigInt_t, Top),
      List(Bot, Top, Top, Top, Top, Top, Top)
    )

  private val absTimes: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot, Bot, Bot),
      List(Bot, Bot, Bot, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bot, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bot, Int_t, BigInt_t, BigInt_t, Top),
      List(Bot, Bot, Bot, BigInt_t, BigInt_t, BigInt_t, Top),
      List(Bot, Bot, Bot, BigInt_t, BigInt_t, BigInt_t, Top),
      List(Bot, Top, Top, Top, Top, Top, Top)
    )

  private val absDivide: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot, Bot, Bot),
      List(Bot, Bot, Bot, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bot, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bot, Int_t, BigInt_t, BigInt_t, Top),
      List(Bot, Bot, Bot, BigInt_t, BigInt_t, BigInt_t, Top),
      List(Bot, Bot, Bot, BigInt_t, BigInt_t, BigInt_t, Top),
      List(Bot, Top, Top, Top, Top, Top, Top)
    )

  private val absGt: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot, Bot, Bot),
      List(Bot, Bot, Bot, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bot, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bot, Bool_t, Bool_t, Bool_t, Top),
      List(Bot, Bot, Bot, Bool_t, Bool_t, Bool_t, Top),
      List(Bot, Bot, Bot, Bool_t, Bool_t, Bool_t, Top),
      List(Bot, Top, Top, Top, Top, Top, Top)
    )

  private val absEq: List[List[Element]] =
    List(
      List(Bot, Bot, Bot, Bot, Bot, Bot, Bot),
      List(Bot, Bool_t, Bot, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bool_t, Bot, Bot, Bot, Top),
      List(Bot, Bot, Bot, Bool_t, Bool_t, Bool_t, Top),
      List(Bot, Bot, Bot, Bool_t, Bool_t, Bool_t, Top),
      List(Bot, Bot, Bot, Bool_t, Bool_t, Bool_t, Top),
      List(Bot, Top, Top, Top, Top, Top, Top)
    )

  def num(i: Int): Element = {
    println(s"called num for $i")
    if (i > Byte.MinValue && i < Byte.MaxValue) Byte_t
    else if (i > Int.MinValue && i < Int.MaxValue) Int_t
    else BigInt_t
  }

  def plus(a: Element, b: Element): Element = lookup(absPlus, a, b)

  def minus(a: Element, b: Element): Element = {
    println(s">>>minus for ${(a, b)}")
    lookup(absMinus, a, b)
  }

  def times(a: Element, b: Element): Element = lookup(absTimes, a, b)

  def div(a: Element, b: Element): Element = lookup(absDivide, a, b)

  def eqq(a: Element, b: Element): Element = lookup(absEq, a, b)

  def gt(a: Element, b: Element): Element = lookup(absGt, a, b)
}
