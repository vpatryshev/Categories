package math.cat



import org.specs2._
import math.cat.PoSet;

import Base._
import static math.cat.PoSet.*;

public class PoSetTest extends TestCase {
  public void testParse() {
    PoSet<String> testPoset = new PoSet<String>(Set("abc", "def", "ab", "defgh")) {
      public boolean _le_(String a, String  b) {
        return b.indexOf(a) >= 0;
      }
    };
    assertEquals(testPoset, PoSet("[abc, def, defgh, ab]{abc<=abc, def<=def, def<=defgh, defgh<=defgh, ab<=abc, ab<=ab}"));
  }
}
