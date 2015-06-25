package j.math.topos;

import java.util.Map;
import java.util.Iterator;
//import com.myjavatools.xml.XmlData;


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
    for (Iterator<String> i = source.objects.keySet().iterator(); i.hasNext();) {
      result.addObject(i.next());
    }

    return result;
  }

  public static Category op(Category source) {
    Category result = new Category(source.name + ".op");

    for (Iterator<String> i = source.objects.keySet().iterator(); i.hasNext();) {
      result.addObject(i.next());
    }

    for (Iterator<Category.Morphism> i = source.morphisms.values().iterator(); i.hasNext();) {
      Category.Morphism f = i.next();
      result.addMorphism(f.name, f.d1.name, f.d0.name);
    }

    for (Iterator<Map.Entry<Category.Morphism[], Category.Morphism>> i = source.mult.entrySet().iterator(); i.hasNext();) {
      Map.Entry<Category.Morphism[], Category.Morphism> entry = i.next();
      Category.Morphism[] from = entry.getKey();
      result.setMult(from[0].name, from[1].name, entry.getValue().name);
    }

    return result;
  }

  private static String pair(String x, String y) {
    return "(" + x + ", " + y + ")";
  }

  public static Category product(Category X, Category Y) {
    Category result = new Category("(" + X.name + " x " + Y.name + ")");

    for (Iterator<String> i = X.objects.keySet().iterator(); i.hasNext();) {
      String xName = i.next();
      for (Iterator<String> j = Y.objects.keySet().iterator(); j.hasNext();) {
        result.addObject(pair(xName, j.next()));
      }
    }

    for (Iterator<Category.Morphism> i = X.morphisms.values().iterator(); i.hasNext();) {
      Category.Morphism f = i.next();
      for (Iterator<Category.Morphism> j = Y.morphisms.values().iterator(); j.hasNext();) {
        Category.Morphism g = j.next();
        result.addMorphism(pair(f.name, g.name),
                           pair(f.d0.name, g.d0.name),
                           pair(f.d1.name, g.d1.name));
      }
    }

    for (Iterator<Map.Entry<Category.Morphism[], Category.Morphism>> i = X.mult.entrySet().iterator(); i.hasNext();) {
      Map.Entry<Category.Morphism[], Category.Morphism> xEntry = i.next();
      Category.Morphism[] xFrom = xEntry.getKey();
      String xh = xEntry.getValue().name;
      for (Iterator<Map.Entry<Category.Morphism[], Category.Morphism>> j = Y.mult.entrySet().iterator(); j.hasNext();) {
        Map.Entry<Category.Morphism[], Category.Morphism> yEntry = j.next();
        Category.Morphism[] yFrom = yEntry.getKey();
        String yh = yEntry.getValue().name;
        result.setMult(pair(xFrom[0].name, yFrom[0].name),
                       pair(xFrom[1].name, yFrom[1].name),
                       pair(xh, yh));
      }
    }

    return result;
  }
}