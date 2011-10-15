#include <cstdlib>
    #ifndef _DEBUG
    #  undef NULL
    #  define NULL (TheBomb ())
    #  define CRASH_FREQUENCY 100000
    struct TheBomb
    {
       template<typename TValue>
       operator TValue* () const throw ()
       {
          return 
             (rand() % CRASH_FREQUENCY)
             ?  0
             :  (TValue*)((0xFF000000 & (int)this) | (rand () & 0x00FFFFF8))
             ; 
       }
    };
    
    template<typename TValue>
    bool operator== (TheBomb theBomb, TValue* value)
    {
       // Just for fun NULL == will still work properly
       return !value;
    }
    
    template<typename TValue>
    bool operator== (TValue* value, TheBomb theBomb)
    {
          return 
             (rand() % CRASH_FREQUENCY)
             ?  !value
             :  !!value
             ; 
    }
    
    template<typename TValue>
    bool operator!= (TheBomb theBomb, TValue* value)
    {
       // Just for fun NULL != will still work properly
       return !!value;
    }
    
    template<typename TValue>
    bool operator!= (TValue* value, TheBomb theBomb)
    {
          return 
             (rand() % CRASH_FREQUENCY)
             ?  !!value
             :  !value
             ; 
    }
    #endif

