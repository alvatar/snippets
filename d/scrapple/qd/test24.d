module test24;

import qd, SDL_image;
import tools.log;

interface Draggable {
  void pickUp(pt);
  void drag(pt);
  void drop(pt);
  void setDragDg(void delegate(ref proc));
}

interface Drawable {
  void draw();
}

interface DropTarget {
  bool accept(Draggable droppings, ref pt where);
}

struct DragDropFuns(DragDropDesc) {
  static {
    bool delegate(pt)[Draggable] drags;
    Draggable selected;
    
    void draw() {
      if (auto draw = cast(Drawable) selected)
        draw.draw();
    }
    void checkEvents() {
      if (selected) { // currently dragging
        if (DragDropDesc.stopDrag()) {
          selected.drop(mouse.pos);
          selected = null;
        } else if (mouse.moved) {
          selected.drag(mouse.pos);
        }
      } else { // not currently dragging and not currently clicking
        if (DragDropDesc.startDrag()) {
          Draggable target;
          foreach (drag, dg; drags) {
            if (dg(mouse.pos)) {
              target = drag;
              break;
            }
          }
          if (!target) return;
          drags.remove(target);
          selected = target;
          selected.pickUp(mouse.pos);
        }
      }
    }
  }
}

struct DropTargets {
  static {
    bool delegate(pt)[DropTarget] targets;
    
    void opIndexAssign(bool delegate(pt) dg, DropTarget dt) { targets[dt] = dg; }
    bool checkDrop(Draggable dr, ref pt p) {
      foreach (target, dg; targets) {
        if (dg(p) && target.accept(dr, p)) return true;
      }
      return false;
    }
  }
}

struct ClickDragRelease {
  static {
    bool startDrag() { return mouse.clicked(Button.Left); }
    bool stopDrag() { return mouse.released(Button.Left); }
  }
}

struct ClickMoveClick {
  static {
    bool startDrag() { return mouse.clicked(Button.Left); }
    bool stopDrag() { return mouse.clicked(Button.Left); }
  }
}

struct HybridDrag {
  static {
    pt startPt; bool dragging;
    bool startDrag() {
      if (mouse.clicked(Button.Left)) {
        startPt = mouse.pos;
        return true;
      }
      return false;
    }
    bool stopDrag() {
      auto
        distpt = mouse.pos - startPt,
        distval = abs(distpt.x) + abs(distpt.y);
      if (distval > 16 && mouse.released(Button.Left)) return true;
      return mouse.clicked(Button.Left);
    }
  }
}

// alias DragDropFuns!(ClickDragRelease) DragDrop;
// alias DragDropFuns!(ClickMoveClick) DragDrop;
alias DragDropFuns!(HybridDrag) DragDrop;

template castCall(T, string MEMBER) {
  void castCall(S)(S[] input) {
    foreach (value; input)
      if (auto asTee = cast(T) value)
        mixin("asTee."~MEMBER~"; ");
  }
}

class Grid : Drawable, DropTarget {
  Area area;
  int w, h;
  Draggable[] field;
  int draggingOver = -1;
  this(int w, int h) {
    this.w = w; this.h = h;
    // snap from outside
    area = display.select(32-16, 32-16, 32*w - 1, 32*h - 1);
    DropTargets[this] = area /apply/ (Area a, pt p) {
      return !!(p in a);
    };
    field = new Draggable[w * h];
  }
  override { // Drawable
    void draw() {
      for (int x = 0; x < w; x ++) {
        for (int y = 0; y < h; y ++) {
          auto sx = x * 32 + 32, sy = y * 32 + 32;
          auto id = y * w + x;
          if (id == draggingOver) line(sx, sy, sx + 32, sy + 32,
            Box=Black, Fill=Black~White~White);
          else line(sx, sy, sx + 32, sy + 32, Box=Black);
        }
      }
      castCall!(Drawable, "draw()")(field);
    }
  }
  pt snap(pt p) {
    pt pp;
    pp.x = (p.x + 16) & ~31;
    pp.y = (p.y + 16) & ~31;
    return pp;
  }
  int toID(pt p) {
    p.x = (p.x - area.tl.x) / 32;
    p.y = (p.y - area.tl.y) / 32;
    return p.y * w + p.x;
  }
  bool accept(Draggable dropping, ref pt p) {
    auto pp = snap(p), id = toID(pp);
    // we already know we're in the area
    if (field[id]) return false;
    p = pp;
    field[id] = dropping;
    dropping.setDragDg((ref proc dg) {
      dg = stuple(this, id) /apply/ (Grid That, int id) {
        That.field[id] = null;
      };
    });
    return true;
  }
}

template DefaultDrag(alias SIZE) {
  pt pos, dragStart, dragCursorStart;
  proc onDrag;
  void setDragArea() {
    DragDrop.drags[this] = display.select(pos.x, pos.y, SIZE.x, SIZE.y)
      /apply/ (Area a, pt p) { return !!(p in a); };
  }
  override {
    void setDragDg(void delegate(ref proc) dg) { dg(onDrag); }
    void pickUp(pt p) {
      dragStart = pos;
      dragCursorStart = p;
      onDrag();
    }
    void drag(pt p) {
      pos = dragStart + (p - dragCursorStart);
    }
    void drop(pt p) {
      pos = dragStart + (p - dragCursorStart);
      if (!DropTargets.checkDrop(this, pos)) {
        // Time to die, Mr. Anderson.
        // delete this;
        return;
      }
      setDragArea;
    }
  }
}

class Icon : Image, Draggable, Drawable {
  mixin DefaultDrag!(dimensions);
  this(void[] data, pt pos) {
    super(data);
    this.pos = pos;
    setDragArea;
  }
  override void draw() { display.select(pos.x, pos.y).blit(this); }
}

class GroupIcon : Draggable, Drawable {
  Area area;
  pt pos, dragStart, dragCursorStart;
  proc onDrag;
  void setDragArea() {
    DragDrop.drags[this] = display.select(pos.x, pos.y, 32, 32)
      /apply/ (Area a, pt p) { return !!(p in a); };
  }
  bool follower, dropFailed;
  override {
    void setDragDg(void delegate(ref proc) dg) { dg(onDrag); }
    void pickUp(pt p) {
      dragStart = pos;
      dragCursorStart = p;
      onDrag();
      if (follower) return;
      foreach (bro; brothers) {
        bro.follower = true;
        bro.pickUp(p + (bro.pos - pos));
        DragDrop.drags.remove(bro);
      }
    }
    void drag(pt p) {
      auto oldpos = pos;
      pos = dragStart + (p - dragCursorStart);
      if (follower) return;
      foreach (bro; brothers)
        bro.drag(p + (bro.pos - oldpos));
    }
    void drop(pt p) {
      auto oldpos = pos;
      pos = dragStart + (p - dragCursorStart);
      if (!DropTargets.checkDrop(this, pos)) {
        dropFailed = true;
      }
      if (follower) return;
      bool oneFailed = dropFailed;
      foreach (bro; brothers) {
        bro.drop(p + (bro.pos - oldpos));
        if (bro.dropFailed) oneFailed = true;
      }
      if (oneFailed) { // IF ONE OF US FAILS, ALL OF US FAIL
        foreach (bro; brothers) {
          if (!bro.dropFailed) {
            bro.onDrag(); // drag body into the grave
            delete bro;
          }
        }
        if (!dropFailed) onDrag(); // drag ourselves loose.
        delete this;
      } else {
        foreach (bro; brothers) {
          bro.setDragArea;
          bro.follower = false;
        }
        setDragArea;
      }
    }
    void draw() {
      display.select(pos.x, pos.y).blit(area);
      if (follower) return;
      foreach (bro; brothers)
        if (bro.follower)
          bro.draw();
    }
  }
  GroupIcon[] brothers;
  void linkUp(GroupIcon[] family) {
    foreach (gi; family)
      if (gi !is this)
        brothers ~= gi;
  }
  this(Area area, pt pos) {
    this.area = area;
    this.pos = pos;
    setDragArea;
  }
}

GroupIcon[] mkGroup(Area area, pt p) {
  auto w = area.width / 32, h = area.height / 32;
  GroupIcon[] res;
  for (int x = 0; x < w; ++x)
    for (int y = 0; y < h; ++y)
      res ~= new GroupIcon(area.select(x * 32, y * 32, 32, 32), p + pt(x * 32, y * 32));
  foreach (gi; res)
    gi.linkUp(res);
  return res;
}

void main() {
  screen(640, 480);
  GroupIcon delegate(pt) genPizza; GroupIcon[] delegate() genPizzaz;
  GroupIcon[] pizzaz;
  void servePizza() {
    pizzaz = genPizzaz();
  }
  void setServe(ref proc dg) { dg = &servePizza; };
  auto pipes = new Image(import("straightpipe.png"));
  genPizzaz = {
    auto res = mkGroup(pipes, pt(312, 32));
    foreach (icon; res)
      icon.setDragDg(&setServe);
    return res;
  };
  servePizza;
  auto grid = new Grid(7, 7);
  while (true) {
    cls(White);
    grid.draw;
    foreach (pizza; pizzaz)
      pizza.draw;
    DragDrop.draw;
    flip;
    events;
    DragDrop.checkEvents;
    if (DragDrop.selected && key.pressed(SDLKey.LCtrl)) {
      logln("Rotate pic!");
      DragDrop.selected = null;
      auto npipes = pipes.rot_right();
      
    }
  }
}
