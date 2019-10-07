
CONSTANT copyright = "bdl2genero by N.J.Martin neilm@4js-emea.com"
CONSTANT version = "0.6"

&define ADDRULE(a,v) \
        LET rule = actions.createChild("Rule") \
        CALL rule.setAttribute("key",a) \
        CALL rule.setAttribute("action",v)

&define DBG(m) \
        LET dbgstr =  __LINE__ USING "####"," ",__FILE__ \
        DISPLAY dbgstr,": ",m CLIPPED \

