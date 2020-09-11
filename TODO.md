* FFI (Where do effect implementations live?) Folders marked as pure or not.
* Export
* Std Lib
* Blackbox Demo
  * Suspend
  * Hello world (asking for name)
  * Forking
* Race parallel computations

b
```
chain(eff,f)={
    Effect(eff.data,{r=>
      eff.onSuccess(r)
    }, eff.onFail )
}
import "predef/eff" ^{};
{
 chain = fun(ma,f)

}




```

