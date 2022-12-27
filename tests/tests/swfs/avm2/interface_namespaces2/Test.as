// compiled with mxmlc

package {
    import flash.display.MovieClip;

    public class Test extends MovieClip {
        public function Test() {

        }
    }
}

trace("== Extending interfaces ==")

interface I1 {
	function func1();
}
interface I2 extends I1 {
	function func2();	
}
class C implements I2 {
	public function func1() { trace("called 1"); }
	public function func2() { trace("called 2"); }
}
var c1: I1 = new C();
var c2: I2 = new C();
var c3: C = new C();
c1.func1();
c2.func1();
c2.func2();
c3.func1();
c3.func2();

trace("== Namespace collision edge cases ==")

namespace ns = "Interface";
interface Interface {
	function func1();
	function func2();
}
class Cls implements Interface {
	public function func1(){
		trace("public func1");
	}
	ns function func1(){
		trace("ns func1");
	}
	public function func2(){
		trace("public func2");
	}
	ns function get func2(){
		trace("ns func2");
	}
}
var b1: Interface = new Cls();
var b2: Cls = new Cls();
var b3 = new Cls();

// interface didn't override ns::func1 so this looks up the non-overridden one
b1.func1();
b2.func1();
b3.func1();
// but here interface DID override ns::func1, as the trait type didn't match
b1.func2();
b2.func2();
b3.func2();