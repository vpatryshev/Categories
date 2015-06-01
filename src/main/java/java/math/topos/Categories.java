package java.math.topos;

import java.util.Collection;
import java.util.Map;
import java.util.Iterator;
//import com.myjavatools.xml.XmlData;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * <p>Title: Topos project</p>
 * <p>Description: Categories, Functors, Topologies</p>
 * <p>Copyright: Copyright (c) 2003 Vlad Patryshev</p>
 * <p>Company: </p>
 * @author Vlad Patryshev
 * @version 1.0
 */

public class Categories {

  public static Category discreteCategoryFactory(int n) {
    Category result = new Category("" + n + "x1");

    for (int i = 0; i < n; i++) {
      result.addObject("" + i);
    }
    return result;
  }

  public static Category linearCategoryFactory(int n) {
    Category result = new Category("" + n);

    for (int i = 0; i < n; i++) {
      result.addObject("" + i);
    }

    Category.Morphism table[][] = new Category.Morphism[n][];
    for (int i = 0; i < n - 1; i++) {
      table[i] = new Category.Morphism[n];
      for (int j = i + 1; j < n; j++) {
        table[i][j] = result.new Morphism("[" + i + "," + j + "]", "" + i, "" + j);
        result.addMorphism(table[i][j]);
        for (int k = 0; k < i; k++) {
          result.setMult(table[k][i], table[i][j], table[k][j]);
        }
      }
    }

    return result;
  }

  public static Category discrete(Category source) {
    Category result = new Category(source.name + ".discrete");
    for (Iterator i = source.objects.keySet().iterator(); i.hasNext();) {
      result.addObject((String)i.next());
    }

    return result;
  }

  public static Category op(Category source) {
    Category result = new Category(source.name + ".op");

    for (Iterator i = source.objects.keySet().iterator(); i.hasNext();) {
      result.addObject((String)i.next());
    }

    for (Iterator i = source.morphisms.values().iterator(); i.hasNext();) {
      Category.Morphism f = (Category.Morphism)i.next();
      result.addMorphism(f.name, f.d1.name, f.d0.name);
    }

    for (Iterator i = source.mult.entrySet().iterator(); i.hasNext();) {
      Map.Entry entry = (Map.Entry)i.next();
      Category.Morphism[] from = (Category.Morphism[])entry.getKey();
      result.setMult(from[0].name, from[1].name, ((Category.Morphism)entry.getValue()).name);
    }

    return result;
  }

  private static String pair(String x, String y) {
    return "(" + x + ", " + y + ")";
  }

  public static Category product(Category X, Category Y) {
    Category result = new Category("(" + X.name + " x " + Y.name + ")");

    for (Iterator i = X.objects.keySet().iterator(); i.hasNext();) {
      String xName = (String)i.next();
      for (Iterator j = Y.objects.keySet().iterator(); j.hasNext();) {
        result.addObject(pair(xName, (String)j.next()));
      }
    }

    for (Iterator i = X.morphisms.values().iterator(); i.hasNext();) {
      Category.Morphism f = (Category.Morphism)i.next();
      for (Iterator j = Y.morphisms.values().iterator(); j.hasNext();) {
        Category.Morphism g = (Category.Morphism) j.next();
        result.addMorphism(pair(f.name, g.name),
                           pair(f.d0.name, g.d0.name),
                           pair(f.d1.name, g.d1.name));
      }
    }

    for (Iterator i = X.mult.entrySet().iterator(); i.hasNext();) {
      Map.Entry xEntry = (Map.Entry)i.next();
      Category.Morphism[] xFrom = (Category.Morphism[])xEntry.getKey();
      String xh = ((Category.Morphism)xEntry.getValue()).name;
      for (Iterator j = Y.mult.entrySet().iterator(); j.hasNext();) {
        Map.Entry yEntry = (Map.Entry) j.next();
        Category.Morphism[] yFrom = (Category.Morphism[]) yEntry.getKey();
        String yh = ( (Category.Morphism) yEntry.getValue()).name;
        result.setMult(pair(xFrom[0].name, yFrom[0].name),
                       pair(xFrom[1].name, yFrom[1].name),
                       pair(xh, yh));
      }
    }

    return result;
  }
}