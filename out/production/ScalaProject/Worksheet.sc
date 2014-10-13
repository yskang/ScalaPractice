class C {
  var acc = 0;
  def minc():Unit = { acc += 1}
  val finc: ()=>Unit = { () => acc += 1}
}

val c = new C

val x = c.minc _

c.minc
c.acc


c.finc.apply()
c.acc

c.minc
c.acc

c.finc
c.acc

c.finc
c.acc
