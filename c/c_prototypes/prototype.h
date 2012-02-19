/* (c) Chris O'Hara 2011 - MIT License */
/* Language hack alert - don't use this for anything ever. */

#ifndef __PROTOTYPE_H
#define __PROTOTYPE_H

#if !defined(__GNUC__) || __STDC_VERSION__ != 199901L
# error GNU99 is required
#endif

#ifndef PROTOTYPE_CALLOC
#define PROTOTYPE_CALLOC calloc
#endif

#ifndef PROTOTYPE_FREE
#define PROTOTYPE_FREE free
#endif

#define INTERFACE(methods) \
  typedef struct proto_s Prototype; \
  struct proto_s { Prototype *proto; methods }

#define PROTOTYPE(prototype, members, ...) \
  typedef struct prototype##_s prototype; \
  struct prototype##_s { Prototype proto; members }; \
  Prototype prototype##Proto = { .proto = NULL, ##__VA_ARGS__ }

#define PROTOTYPE_INHERIT(prototype, parent, members, ...) \
  typedef struct prototype##_s prototype; \
  struct prototype##_s { Prototype proto; members }; \
  Prototype prototype##Proto = { .proto = &parent##Proto, ##__VA_ARGS__ }

#define NEW(Obj) \
  ({ Prototype *proto = PROTOTYPE_CALLOC(sizeof(Obj), 1); \
     *proto = Obj##Proto; \
     (Obj *) proto; })

#define DESTROY(Obj) \
  (PROTOTYPE_FREE((void *)Obj))

#define CONSTRUCT(Obj, ...) \
  ({ Prototype *obj = (void *)NEW(Obj), *proto = obj; \
     while (proto != NULL && proto->construct == NULL) \
         proto = proto->proto; \
     if (proto->construct) \
         proto->construct(proto, ##__VA_ARGS__); \
     (Obj *) obj; })

#define DESTRUCT(Obj, ...) \
  do { Prototype *obj = (void *)Obj, *proto = obj; \
     while (proto != NULL && proto->destruct == NULL) \
         proto = proto->proto; \
     if (proto && proto->destruct) \
         proto->destruct(proto, ##__VA_ARGS__); \
     DESTROY(obj); } while (0)

#define CALL(Obj, method, ...) \
  (((Prototype *)(void *)Obj)->method \
    ? ((Prototype *)(void *)Obj)->method(Obj, ##__VA_ARGS__) \
    : ({ Prototype *proto = (void *)Obj, *top = proto; \
         while (proto != NULL && proto->method == NULL) \
            proto = proto->proto; \
         proto && (top->method = proto->method) \
            ? proto->method(Obj, ##__VA_ARGS__) : 0; }))

#define SUPER(Obj, method, ...) \
    CALL(((Prototype *)(void *)Obj)->proto, method, ##__VA_ARGS__)

#define GET(Obj, member) \
  (((Prototype *)(void *)Obj)->member \
    ? ((Prototype *)(void *)Obj)->member \
    : ({ Prototype *proto = (void *)Obj, *top = proto; \
         while (proto != NULL && proto->member == NULL) \
            proto = proto->proto; \
         proto && (top->member = proto->member) \
            ? proto->member : 0; }))

#define SET(Obj, member, value) \
  (((Prototype *)(void *)Obj)->member = value)

#endif
