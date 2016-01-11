typedef int __ssize_t;
struct __anonstruct_pthread_mutex_t_6
{
};
typedef struct __anonstruct_pthread_mutex_t_6 pthread_mutex_t;
typedef struct __anonstruct_pthread_mutexattr_t_7 pthread_mutexattr_t;
typedef __ssize_t ssize_t;
typedef struct cache_entry cache_entry;
#pragma existential(cache_entry, "cache_entry.refs_mutex", "&cache_entry.refs")
struct cache_entry
{
  int refs;
  pthread_mutex_t refs_mutex;
  cache_entry *prev;
};
struct __pthread_attr_s
{
};
typedef struct __pthread_attr_s pthread_attr_t;
typedef unsigned long pthread_t;
pthread_mutex_t g_cache_mutex;
static int g_cache_cur = 0;
static void
cache_remove (cache_entry * entry)
{
  start_unpack (entry);
  {
	if ( entry->prev != ((void *) 0))
	  {
	    g_cache_cur++;
	  }
  }
  end_unpack(entry);
}
static void
cache_evict (void)
{
/*   struct cache_entry __attribute__((packed))* result = */
/*     (cache_entry __attribute__((packed)) *)malloc(sizeof((*result))); */
/*   cache_entry *remove___0 = pack(result); */
  cache_entry *remove___0;
  {
	cache_remove (remove___0);
	cache_entry_release (remove___0);
  }
}
static void
cache_finish_get (cache_entry * entry)
{
  {
    cache_evict ();
  }
}
void
cache_entry_release (cache_entry * entry)
{
}
pthread_mutex_t g_cache_mutex;
cache_entry *
get_request_entry (void)
{
  cache_entry *tmp___0;
  {
	    pthread_mutex_lock (&g_cache_mutex);
	    cache_finish_get (tmp___0);
	    pthread_mutex_unlock (&g_cache_mutex);
    return (tmp___0);
  }
}
void *
thread_process_client (void *client)
{
    cache_entry_release (get_request_entry ());
}
static pthread_t thread;
void
accept_loop (int id, int s)
{
  int c;
  int tmp___0;
  {
    while (1)
      {
	    tmp___0 =
	      pthread_create ((pthread_t * __restrict) (&thread),
			      (void *)0,
			      &thread_process_client, 0);
      }
    pthread_mutex_init ((pthread_mutex_t * __restrict) (&g_cache_mutex),
			(pthread_mutexattr_t const *__restrict) ((void *) 0));
  }
}
