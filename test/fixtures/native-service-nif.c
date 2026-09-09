#include <erl_nif.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
typedef struct { atomic_int closed; } Resource;
static ErlNifResourceType *type;
static atomic_int closes, destructors;
static void close_once(Resource *r) { if(!atomic_exchange(&r->closed,1)) atomic_fetch_add(&closes,1); }
static void destroy(ErlNifEnv *env, void *p) { (void)env; close_once(p); atomic_fetch_add(&destructors,1); }
static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info) {
 (void)priv; (void)info;
 type=enif_open_resource_type(env,NULL,"owned_float",destroy,ERL_NIF_RT_CREATE,NULL);
 return type ? 0 : -1;
}
static ERL_NIF_TERM open_resource(ErlNifEnv *env,int argc,const ERL_NIF_TERM argv[]) {
 (void)argc; (void)argv;
 Resource *r=enif_alloc_resource(type,sizeof(*r)); if(!r) return enif_make_badarg(env);
 atomic_init(&r->closed,0); ERL_NIF_TERM term=enif_make_resource(env,r); enif_release_resource(r); return term;
}
static ERL_NIF_TERM close_resource(ErlNifEnv *env,int argc,const ERL_NIF_TERM argv[]) {
 Resource *r; if(argc!=1 || !enif_get_resource(env,argv[0],type,(void**)&r)) return enif_make_badarg(env);
 close_once(r); return enif_make_atom(env,"ok");
}
static ERL_NIF_TERM call(ErlNifEnv *env,int argc,const ERL_NIF_TERM argv[]) {
 Resource *r; double x;
 if(argc!=2 || !enif_get_resource(env,argv[0],type,(void**)&r) || atomic_load(&r->closed) || !enif_get_double(env,argv[1],&x)) return enif_make_badarg(env);
 if(x==91.0) abort(); /* Only invoked by the isolated crashing-VM test. */
 if(x==92.0) { clock_t start=clock(); while((double)(clock()-start)/CLOCKS_PER_SEC<0.2) {} }
 if(x==93.0) return enif_make_double(env,INFINITY);
 if(x==94.0) return enif_make_double(env,NAN);
 if(x==95.0) return enif_make_tuple2(env,enif_make_atom(env,"wrong_scheduler"),enif_make_double(env,x));
 ERL_NIF_TERM scheduler=enif_make_atom(env,enif_thread_type()==ERL_NIF_THR_DIRTY_CPU_SCHEDULER ? "dirty_cpu" : "wrong_scheduler");
 return enif_make_tuple2(env,scheduler,enif_make_double(env,x));
}
static ERL_NIF_TERM stats(ErlNifEnv *env,int argc,const ERL_NIF_TERM argv[]) {
 (void)argc; (void)argv;
 return enif_make_tuple2(env,enif_make_int(env,atomic_load(&closes)),enif_make_int(env,atomic_load(&destructors)));
}
static ErlNifFunc functions[]={
 {"open",0,open_resource,ERL_NIF_DIRTY_JOB_CPU_BOUND},
 {"close",1,close_resource,ERL_NIF_DIRTY_JOB_CPU_BOUND},
 {"call",2,call,ERL_NIF_DIRTY_JOB_CPU_BOUND},
 {"stats",0,stats,0}
};
ERL_NIF_INIT(catena_native_service,functions,load,NULL,NULL,NULL)
