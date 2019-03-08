package j.math.topos;

import java.util.Map;
import java.util.Iterator;
//import com.myjavatools.xml.XmlData;
import java.util.HashMap;

/**
 * <p>Title: Topos project</p>
 * <p>Description: Categories, Functors, Topologies</p>
 * <p>Copyright: Copyright (c) 2003 Vlad Patryshev</p>
 * <p>Company: </p>
 *
 * @version 1.0
 */

public class Category {

  private class Entity {
    public final String name;
    public Entity(String name) {
      this.name = name;
    }
  }

  public class Object extends Entity {
    public final Morphism id;

    public Object(String name) {
      super(name);
      id = new Morphism("1(" + name + ")", this, this);
    }

    public String toString() {
      return "Object " + name;
    }
  }

  public class Morphism extends Entity {
    public final Object d0;
    public final Object d1;

    public Morphism(String name, Object d0, Object d1) {
      super(name);
      this.d0 = d0;
      this.d1 = d1;
    }

    public Morphism(String name, String d0, String d1) {
      this(name, objects.get(d0), objects.get(d1));
    }

    public String toString() {
      return "Morphism " + name + ": " + d0 + " -> " + d1;
    }
  }

  public Morphism mult(Morphism a, Morphism b) {
    return mult.get(new Morphism[] {a, b});
  }

  public final String name;
  Map<String, Object> objects     = new HashMap<String, Object>();
  Map<String, Morphism> morphisms = new HashMap<String, Morphism>();
  Map<Morphism[], Morphism> mult  = new HashMap<Morphism[], Morphism>();

  void setMult(Morphism a, Morphism b, Morphism c) {
    mult.put(new Morphism[] {a, b}, c);
  }

  void setMult(String a, String b, String c) {
    mult.put(new Morphism[] {morphisms.get(a), morphisms.get(b)},
             morphisms.get(c));
  }

  public String validateIds() {
    for (Iterator<Object> object = objects.values().iterator(); object.hasNext();) {
      Object o = object.next();
      if (o.id == null) {
        return "Error: Missing id morphism for object " + o;
      }

      if (o.id.d0 != o) {
        return "Error: Wrong id morphism for object " + o + " - its d0 is " + o.id.d0;
      }

      if (o.id.d1 != o) {
        return "Error: Wrong id morphism for object " + o + " - its d1 is " + o.id.d1;
      }

      for (Iterator<Morphism> morphism = morphisms.values().iterator(); morphism.hasNext();) {
        Morphism m = morphism.next();
        if (m.d0 == o) {
          Morphism mult = mult(o.id, m);
          if (mult != m) {
            return "Error: Wrong id morphism for object " + o + " - " +
                   o.id + " * " + m + " = " + mult + " ( must be " + m + ")";
          }
        }

        if (m.d1 == o) {
          Morphism mult = mult(m, o.id);
          if (mult != m) {
            return "Error: Wrong id morphism for object " + o + " - " +
                   m + " * " + o.id + " = " + mult + " ( must be " + m + ")";
          }
        }
      }
    }
    return null;
  }

  public String validateMultiplication() {
    for (Iterator<Morphism> first = morphisms.values().iterator(); first.hasNext();) {
      Morphism f = first.next();
      for (Iterator<Morphism> second = morphisms.values().iterator(); second.hasNext();) {
        Morphism g = second.next();
        if (f.d1 != g.d0) continue;

        Morphism fg = mult(f, g);

        if (fg.d0 != f.d0 || fg.d1 != g.d1) {
          return "Error: " + f + " o " + g + " = " + fg;
        }

        for (Iterator<Morphism> third = morphisms.values().iterator(); third.hasNext();) {
          Morphism h = third.next();
          if (g.d1 != h.d0) continue;
          Morphism a = mult(fg, h);
          Morphism b = mult(f, mult(g, h));

          if (a != b) {
            return "Error: broken association law for " + f + ", " + g + " h " +
                   " - got " + a + " and " + b;
          }
        }
      }
     }
    return null;
  }

  private String validateVerbose() {
    String result = validateIds();
    if (result != null) return result;
    result = validateMultiplication();
    if (result != null) return result;
    return null;
  }

  private boolean validate() {
    if (validateIds()            != null) return false;
    if (validateMultiplication() != null) return false;
    return true;
  }

  public Category(String name) {
    this.name = name;
  }
/*
  public Category(XmlData xml) throws InstantiationException {
    name = xml.getName();
    int nObjects = xml.getKidCount("object");
    int nMorphisms = xml.getKidCount("morphism");

    for (int i = 0; i < nObjects; i++) {
      addObject(xml.getKid("object", i).getName());
    }

    for (int i = 0; i < nMorphisms; i++) {
      XmlData source = xml.getKid("morphism", i);
      addMorphism(source.getName(),
                  source.getAttribute("d0"),
                  source.getAttribute("d1"));
    }

    for (int i = 0; i < nMorphisms; i++) {
      XmlData source = xml.getKid("morphism", i);
      Morphism m = (Morphism)morphisms.get(source.getName());

      setMult(m.d0.id, m, m);
      setMult(m, m.d1.id, m);

      int nMult = source.getKidCount("multiply");
      for (int j = 0; j < nMult; j++) {
        XmlData record = source.getKid("multiply", i);
        setMult(m.name, record.getAttribute("by"), record.getAttribute("result"));
      }
    }
    String result = validateVerbose();
    if (result != null) throw new InstantiationException(result);
  }
*/
  /*
  public XmlData toXml() {
    XmlData result = new XmlData("category", null,
                                 new String[] {"name", name});

    for (Iterator i = objects.keySet().iterator(); i.hasNext();) {
      result.addKid(new XmlData("object", null, new String[] {"name", (String)i.next()}));
    }

    for (Iterator i = morphisms.values().iterator(); i.hasNext();) {
      Morphism f = (Morphism)i.next();
      if (f == f.d0.id) continue;
      XmlData morphismData = new XmlData("morphism", null, new String[] {"name", f.name});
      result.addKid(morphismData);

      for (Iterator j = morphisms.values().iterator(); j.hasNext();) {
        Morphism g = (Morphism)j.next();
        if (f.d1 != g.d0) continue;
        morphismData.addKid(new XmlData("multiply", null,
                                        new String[] {"by", g.name,
                                                      "result", mult(f, g).name}));
      }
    }

    return result;
  }
*/
  public void addObject(String name) {
    Object o = new Object(name);
    objects.put(o.name, o);
    morphisms.put(o.id.name, o.id);
    setMult(o.id, o.id, o.id);
  }

  public void addMorphism(Morphism m) {
    morphisms.put(m.name, m);
  }

  public void addMorphism(String name, String d0, String d1) {
    addMorphism(new Morphism(name, d0, d1));
  }

// the following stuff will look better in Categories class

  static Category discreteCategoryFactory(int n) {
    Category result = new Category("" + n + "x1");

    for (int i = 0; i < n; i++) {
      result.addObject("" + i);
    }
    return result;
  }

  static Category linearCategoryFactory(int n) {
    Category result = new Category("" + n);

    for (int i = 0; i < n; i++) {
      result.addObject("" + i);
    }

    Morphism table[][] = new Morphism[n][];
    for (int i = 0; i < n - 1; i++) {
      table[i] = new Morphism[n];
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

  public Category discrete() {
    Category result = new Category(this.name + ".discrete");
    for (Iterator<String> i = objects.keySet().iterator(); i.hasNext();) {
      result.addObject(i.next());
    }

    return result;
  }

  public Category op() {
    Category result = new Category(this.name + ".op");

    for (Iterator<String> i = objects.keySet().iterator(); i.hasNext();) {
      result.addObject(i.next());
    }

    for (Iterator<Morphism> i = morphisms.values().iterator(); i.hasNext();) {
      Morphism m = i.next();
      result.addMorphism(m.name, m.d1.name, m.d0.name);
    }

    for (Iterator<Map.Entry<Morphism[], Morphism>> i = mult.entrySet().iterator(); i.hasNext();) {
      Map.Entry<Morphism[], Morphism> entry = i.next();
      Morphism[] from = entry.getKey();
      result.setMult(from[0].name, from[1].name, entry.getValue().name);
    }

    return result;
  }
}