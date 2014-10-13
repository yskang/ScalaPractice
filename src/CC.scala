/**
 * Created by yongsung.kang on 2014-10-06.
 */
class CC(init:Int) {

    var acc = init;
    def minc():Unit = { acc += 1}
    val finc:CC =>Unit = { (x:CC) => acc += 1}

}
