reset <-
function(){
if (exists("popTT")) tkdestroy(popTT)
if (exists("data1")) tkdestroy(data1);if (exists("data2")) tkdestroy(data2)
if (exists("data3")) tkdestroy(data3);if (exists("data4")) tkdestroy(data4)
if (exists("data5")) tkdestroy(data5);if (exists("data6")) tkdestroy(data6)
if (exists("data7")) tkdestroy(data7);if (exists("data8")) tkdestroy(data8)

setVariables()
}

