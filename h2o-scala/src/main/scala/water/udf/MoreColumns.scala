package water.udf

import java.util.Date
import java.{lang, util}

import water.udf.DataColumns._
import water.udf.fp.{Function => UFunction, Functions}
import water.udf.specialized._
import water.udf.{fp => F}

import scala.collection.JavaConverters._

trait ScalaFactory[JavaType, ScalaType, ColType <: DataColumn[JavaType]] extends Serializable { self: BaseFactory[JavaType,ColType] =>

  import collection.JavaConverters._

  def conv(x:ScalaType): JavaType

  def newColumn[U](xs: Iterable[ScalaType]): ColType = {
    val jl: util.List[JavaType] = xs.toList.map(conv).asJava
    val listFunction: fp.Function[lang.Long, JavaType] = Functions.onList(jl)
    self.newColumn(xs.size, listFunction)
  }

  def newColumn[U](xs: Iterator[ScalaType]): ColType = {
    self.newColumn(xs.size, Functions.onList(xs.toList.map(conv).asJava))
  }
}

class ScalaDoubles extends Doubles with ScalaFactory[java.lang.Double, Double, DataColumn[java.lang.Double]] {
  def newColumn(size: Long, f: Long => Double) = super.newColumn(size, ff1LD(f))
  def newColumnOpt(size: Long, f: Long => Option[Double]) = super.newColumn(size, ff1LDO(f))
  override def conv(x: Double): lang.Double = x

  private implicit def ff1LD(f: Long => Double): UFunction[lang.Long, lang.Double] =
    new UFunction[lang.Long, lang.Double] {
      def apply(x: lang.Long): lang.Double = f(x)
    }

  private implicit def ff1LDO(f: Long => Option[Double]): UFunction[lang.Long, lang.Double] =
    new UFunction[lang.Long, lang.Double] {
      def apply(x: lang.Long): lang.Double = f(x).getOrElse(DoubleNan).asInstanceOf[lang.Double]
    }

  val DoubleNan: Double = Double.NaN
}

/**
  * Scala adapters for h2o3 udf (which is currently in Java)
  */
object MoreColumns extends DataColumns {
  
  val Doubles = new ScalaDoubles
  
  private[MoreColumns] implicit def ff1L[Y](f: Long => Y): UFunction[lang.Long, Y] =
    new UFunction[lang.Long, Y] {
      def apply(x: lang.Long): Y = f(x)
    }

  private[MoreColumns] implicit def ff1LO[Y](f: Long => Option[Y]): UFunction[lang.Long, Y] =
    new UFunction[lang.Long, Y] {
      def apply(x: lang.Long): Y = f(x) getOrElse null.asInstanceOf[Y]
    }

  private[MoreColumns] implicit def ff1LS(f: Long => String): UFunction[lang.Long, lang.String] =
    new UFunction[lang.Long, lang.String] {
      def apply(x: lang.Long): lang.String = f(x)
    }

  private[MoreColumns] implicit def ff1LI(f: Long => Integer): UFunction[lang.Long, lang.Integer] =
    new UFunction[lang.Long, lang.Integer] {
      def apply(x: lang.Long): lang.Integer = f(x)
    }

  /**
    * Magma with neutral element
    *
    * @see https://en.wikipedia.org/wiki/Magma_(algebra)
    * @param zero neutral element
    * @param op binary op (does not 
    * @tparam X a type of data
    */
  case class Magma[X](zero: X, op: X => X => X)
  
  private[MoreColumns] implicit def ff1[X](magma:Magma[X]): F.Foldable[X, X] = {
    new F.Foldable[X, X] {
      override def initial(): X = magma.zero

      override def apply(y: X, x: X): X = magma.op(y)(x)
    }
  }
  

  object Dates extends Dates with ScalaFactory[Date, Date, DataColumn[Date]] {
    def newColumn(size: Long, f: Long => Date) = super.newColumn(size, ff1L(f))
    def newColumnOpt(size: Long, f: Long => Option[Date]) = super.newColumn(size, ff1LO(f))
    override def conv(x: Date): Date = x
  }

  object Strings extends Strings with ScalaFactory[String, String, DataColumn[String]] {
    def newColumn(size: Long, f: Long => String) = super.newColumn(size, ff1LS(f))
    def newColumnOpt(size: Long, f: Long => Option[String]) = super.newColumn(size, ff1LO(f))
    override def conv(x: String): String = x
  }

  class ScalaEnums(domain: Iterable[String]) extends Enums(domain.toArray) with ScalaFactory[java.lang.Integer, Integer, EnumColumn] {
    def newColumn(size: Long, f: Long => Integer) = super.newColumn(size, ff1LI(f))
    def newColumnOpt(size: Long, f: Long => Option[Integer]) = super.newColumn(size, ff1LO(f))
    override def conv(x: Integer): java.lang.Integer = x
  }


  def Enums(domain: Iterable[String]) = new ScalaEnums(domain)

  def foldingColumn[X](init: X, op: X => X => X, components: Column[X]*): Column[X] = foldingColumn(Magma(init, op), components:_*)
  
  def foldingColumn[X](f: Magma[X], components: Column[X]*): Column[X] = 
    new FoldingColumn[X, X] (ff1(f), components:_*)

  class ScalaUnfoldable[X, Y]() extends F.Unfoldable[X, Y] {
    private var f: X => Iterable[Y] = null // java serialization problem
    
    def this(f0: X => Iterable[Y]) {
      this()
      f = f0
    }

    def apply(x: X): java.util.List[Y] = {
      val ys = f(x)
      ys.toList.asJava
    }
  }
  
  private[MoreColumns] def fu1[X, Y](f: X => Iterable[Y]): F.Unfoldable[X, Y] = new ScalaUnfoldable[X, Y](f)
  

  def unfoldingColumn[X, Y](f: X => Iterable[Y], source: Column[X], width: Int) = {
    new UnfoldingColumn[X, Y](fu1[X, Y](f), source, width)
  }

//  def unfoldingFrame[X, Y](f: X => Iterable[Y], source: Column[X], width: Int) = {
//    new UnfoldingFrame[X, Y](fu1[X, Y](f), source, width)
//  }
   
}
