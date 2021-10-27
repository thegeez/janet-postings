function inlineReplace(xhttp,newEl,orig){

    // Typical action to be performed when the document is ready:
    var fullpage = document.createElement('div');
    var rt = xhttp.responseText;
    var sc = rt.split('<section id="inline-main">')[1].split('</section>')[0];
    fullpage.innerHTML = sc;

    newEl.parentNode.replaceChild(fullpage, newEl);

    updateForm(fullpage, orig);
}

function updateForm(el, orig){
    var us = window.location.pathname; // /foo/bar no query or fragment
    var cbts = el.querySelectorAll('a[href="'+us+'"]'); //find cancel button

    var fel = el.querySelectorAll("form")[0];
    var formTarget = fel.getAttribute("action");

    var sbts = el.querySelectorAll('button[type="submit"]'); //find submit button

    cbts.forEach(function(bt) {
        bt.onclick = function(e) {
            e.preventDefault();
            el.innerHTML = orig;
            el.parentNode.replaceChild(orig, el)
            return false;
        };
    });

    sbts.forEach(function(bt) {
        bt.onclick = function(e) {
            e.preventDefault();
            var formData = new FormData(fel);


            var xhttp = new XMLHttpRequest();
            xhttp.onreadystatechange = function() {


                if (this.readyState == 4 && this.status == 201) {
                    // if sent with header 'wa-inline' 303 will be 201
                    window.location = xhttp.getResponseHeader("Location")
                }

                if (this.readyState == 4 && this.status == 200) {
                    //either an error response to render inline

                    // responseURL is full http://..../..path.., formTarget only /..path..
                    if (this.responseURL.endsWith(formTarget)) {
                        // validation error, re-render form
                        inlineReplace(xhttp,el,orig);
                    } else {
                        // or the submit was succesful, and the 303 is followed by XMLHttpRequest (when no 201 created returned)
                        // could put responseText in page, now will request twice (the 200 reponse in the xhttp and this new one below)
                        //window.location = this.responseURL;
                        // need to use response or loose the flash
                        var newBody = "<body"+xhttp.responseText.split("<body")[1].split("</body>")[0];
                        document.querySelectorAll("body")[0].innerHTML = newBody;

                        //make new element also work with form inline
                        runEnhance();
                    }

                }

            };
            var fenc = [];
            formData.forEach(function(value, key){
                fenc.push(""+ key + "=" + encodeURIComponent(value));
            });
            fenc = fenc.join("&");


            xhttp.open("POST", formTarget, true);
            xhttp.setRequestHeader('Content-Type','application/x-www-form-urlencoded');
            xhttp.setRequestHeader("wa-inline", true)
            xhttp.send(fenc);

            return false;
        };
    });
}



function requestInline(el){
    var link = el.getAttribute("href");
    var orig = el;
    var newEl = document.createElement('span');
    newEl.innerHTML = el.innerHTML + "....";

    // replace el with newEL
    el.parentNode.replaceChild(newEl, el);
    
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
                inlineReplace(xhttp,newEl,orig);
            /*
            // Typical action to be performed when the document is ready:
            fullpage = document.createElement('div');
            rt = xhttp.responseText;
            sc = rt.split('<section id="inline-main">')[1].split('</section>')[0];
            fullpage.innerHTML = sc;

            //el.parentNode.replaceChild(fullpage, newEl);
            //newEl.innerHTML =
            newEl.parentNode.replaceChild(fullpage, newEl);

            updateForm(fullpage, orig);
*/
        }
    };
    xhttp.open("GET", link, true);
    xhttp.send();
    
    
}


function runEnhance(){
    var els = document.querySelectorAll("a[wa-inline]");

    els.forEach(function(el) {
        if (!el.getAttribute("wa-enhanced")) {
            el.setAttribute("wa-enhanced", true);
            el.onclick = function(e) {
                e.preventDefault();
                requestInline(el);
                return false;
            };
        };
    });


}
//window.onload=runEnhance();

