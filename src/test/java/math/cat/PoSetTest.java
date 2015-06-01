package math.cat;

import static math.cat.PoSet.PoSet;
import static math.cat.Sets.Set;
import junit.framework.TestCase;

/**
 * Unittest for PoSet class
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
public class PoSetTest extends TestCase {
  public void testParse() {
    PoSet<String> testPoset = new PoSet<String>(Set("abc", "def", "ab", "defgh")) {
      @Override
      public boolean _le_(String a, String b) {
        return b.indexOf(a) >= 0;
      }
    };
    assertEquals(testPoset, PoSet("[abc, def, defgh, ab]{abc<=abc, def<=def, def<=defgh, defgh<=defgh, ab<=abc, ab<=ab}"));
  }
}
