package j.math.cat;

import junit.framework.TestCase;

/**
 * Unittest for PoSet class
 */
public class PoSetTest extends TestCase {
  public void testParse() {
    PoSet<String> testPoset = new PoSet<String>(Sets.Set("abc", "def", "ab", "defgh")) {
      @Override
      public boolean _le_(String a, String b) {
        return b.indexOf(a) >= 0;
      }
    };
    TestCase.assertEquals(testPoset, PoSet.PoSet("[abc, def, defgh, ab]{abc<=abc, def<=def, def<=defgh, defgh<=defgh, ab<=abc, ab<=ab}"));
  }
}
