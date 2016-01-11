/* ---------------------------------------------------------------------- */

/* SAMPLE MAIN() function to "drive the driver" based on reading of
   Linux kernel code (2.6.11).  Pseudo-main() uses three functions:
   module initialization in module_loaded (single-threaded), usage in
   module_activity (multi-threaded), and cleanup in module_cleanup
   (multi-threaded). */

/* ---------------------------------------------------------------------- */

/* This is code that pretends to be the kernel. */

/* As far as I can tell, net_device objects are shared, so that 
   multiple driver calls might access them at once (or even
   elsewhere in the kernel), but that the saving grace is
   many fields are immutable.  So: no lock for this. */
struct net_device dummydev;
spinlock_t openlock;
spinlock_t statslock;

/* Dummy version of kernel function needed to initialize
   the spin locks in the net_device struct */
int register_netdev(struct net_device *dev)
{
  return 0;
}

/* Dummy version of kernel function to allocate the device struct.
   In reality, this is one chunk of memory with the private data
   adjacent to the net_device struct, but we're faking it here. */
struct net_device * __attribute__((unique)) alloc_etherdev(int ign) {
  struct net_device *dev = &dummydev;
  struct net_local *priv = kmalloc(sizeof(struct net_local),0);
  dev->priv = (void *)priv;
  return dev;
}
struct net_device * __attribute__((unique)) alloc_netdev(int sizeof_priv , char const   *name , void (*setup)(struct net_device * ) ) {
  struct net_device *dev = &dummydev;
  struct net_local *priv = kmalloc(sizeof(struct net_local),0);
  dev->priv = (void *)priv;
  setup(dev);
  return dev;
}


/* Code that's called once the module is known to the system;
   could occur in multiple threads.  Simulates the call from
   dev_queue_xmit in net/core/dev.c. */

static void xmit_packet() {
  struct sk_buff *skb = kmalloc(sizeof(struct sk_buff),0); /* thread-local */
  skb->dev = &dummydev;
  dummydev.hard_start_xmit(skb, &dummydev);
  dummydev.timer.function(&dummydev);
} 


static void recv_packet() {
  //call receive function when it exists
}

static void module_activity() {
  struct net_device_stats *stats;

  _spin_lock(&openlock);
  dummydev.open(&dummydev);
  dummydev.stop(&dummydev);
  dummydev.do_ioctl(&dummydev, 0, 0);
  _spin_unlock(&openlock);

  _spin_lock(&dummydev.xmit_lock);
  xmit_packet();
  _spin_unlock(&dummydev.xmit_lock);

  recv_packet(); /* simulates interrupt handler */

  _spin_lock(&statslock);
  stats = dummydev.get_stats(&dummydev);
  _spin_unlock(&statslock);
}

static void dummy_interrupt() {
  // call interrupt handler when one exists
}

typedef int pthread_t;

int main() {
  pthread_t t;
  spin_lock_init(&openlock);
  spin_lock_init(&statslock);
  spin_lock_init(&dummydev.queue_lock);
  spin_lock_init(&dummydev.xmit_lock);
  init_module();
  /* NOTE: I'm not calling wavelan_probe; I don't think it gets
     used when the driver is a module, but I'm not sure. */
  pthread_create(&t, (void*)0, module_activity, (void*)0);
  pthread_create(&t, (void*)0, dummy_interrupt, (void*)0);
  pthread_create(&t, (void*)0, module_activity, (void*)0);
  /* TODO: make this a join()? */
  cleanup_module();
}
