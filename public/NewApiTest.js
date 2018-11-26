!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function e(t){return r(2,t,function(r){return function(n){return t(r,n)}})}function t(e){return r(3,e,function(t){return function(r){return function(n){return e(t,r,n)}}})}function u(u){return r(4,u,function(e){return function(t){return function(r){return function(n){return u(e,t,r,n)}}}})}function a(a){return r(5,a,function(u){return function(e){return function(t){return function(r){return function(n){return a(u,e,t,r,n)}}}}})}function i(i){return r(6,i,function(a){return function(u){return function(e){return function(t){return function(r){return function(n){return i(a,u,e,t,r,n)}}}}}})}function b(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function s(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function l(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function d(n,r,t,e,u,a){return 5===n.a?n.f(r,t,e,u,a):n(r)(t)(e)(u)(a)}function h(n,r,t,e,u,a,i){return 6===n.a?n.f(r,t,e,u,a,i):n(r)(t)(e)(u)(a)(i)}var $={$:0};function g(n,r){return{$:1,a:n,b:r}}var f=e(g);function p(n){for(var r=$,t=n.length;t--;)r=g(n[t],r);return r}function o(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}function c(n,r,t){if("object"!=typeof n)return n===r?0:n<r?-1:1;if(!n.$)return(t=c(n.a,r.a))?t:(t=c(n.b,r.b))?t:c(n.c,r.c);for(;n.b&&r.b&&!(t=c(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var v=0;function m(n,r){return{a:n,b:r}}function y(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}function k(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t=g(n.a,r);n=n.b;for(var e=t;n.b;n=n.b)e=e.b=g(n.a,r);return t}var w=t(function(n,r,t){for(var e=Array(n),u=0;u<n;u++)e[u]=t(r+u);return e}),A=e(function(n,r){for(var t=Array(n),e=0;e<n&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,m(t,r)});function j(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var _=e(function(n,r){var t=r%n;return 0===n?j(11):0<t&&n<0||t<0&&0<n?t+n:t}),T=Math.cos,N=Math.sin;var E=Math.ceil,x=Math.floor,L=Math.round,P=Math.log;var C=e(function(n,r){return r.join(n)});function M(n){return n+""}var q=e(function(n,r){return O(n,D(r))});function O(n,r){switch(n.$){case 3:return"boolean"==typeof r?mr(r):B("a BOOL",r);case 2:return"number"!=typeof r?B("an INT",r):-2147483647<r&&r<2147483647&&(0|r)===r?mr(r):!isFinite(r)||r%1?B("an INT",r):mr(r);case 4:return"number"==typeof r?mr(r):B("a FLOAT",r);case 6:return"string"==typeof r?mr(r):r instanceof String?mr(r+""):B("a STRING",r);case 9:return null===r?mr(n.c):B("null",r);case 5:return mr(R(r));case 7:return Array.isArray(r)?F(n.b,r,p):B("a LIST",r);case 8:return Array.isArray(r)?F(n.b,r,z):B("an ARRAY",r);case 10:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return B("an OBJECT with a field named `"+t+"`",r);var e=O(n.b,r[t]);return yr(e)?e:pr(b(wr,t,e.a));case 11:var u=n.e;if(!Array.isArray(r))return B("an ARRAY",r);if(r.length<=u)return B("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=O(n.b,r[u]);return yr(e)?e:pr(b(Ar,u,e.a));case 12:if("object"!=typeof r||null===r||Array.isArray(r))return B("an OBJECT",r);var a=$;for(var i in r)if(r.hasOwnProperty(i)){e=O(n.b,r[i]);if(!yr(e))return pr(b(wr,i,e.a));a=g(m(i,e.a),a)}return mr(ir(a));case 13:for(var f=n.f,o=n.g,c=0;c<o.length;c++){e=O(o[c],r);if(!yr(e))return e;f=f(e.a)}return mr(f);case 14:e=O(n.b,r);return yr(e)?O(n.h(e.a),r):e;case 15:for(var v=$,s=n.g;s.b;s=s.b){e=O(s.a,r);if(yr(e))return e;v=g(e.a,v)}return pr(jr(ir(v)));case 1:return pr(b(kr,n.a,R(r)));case 0:return mr(n.a)}}function F(n,r,t){for(var e=r.length,u=Array(e),a=0;a<e;a++){var i=O(n,r[a]);if(!yr(i))return pr(b(Ar,a,i.a));u[a]=i.a}return mr(t(u))}function z(r){return b(hr,r.length,function(n){return r[n]})}function B(n,r){return pr(b(kr,"Expecting "+n,R(r)))}function S(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 3:case 2:case 4:case 6:case 5:return!0;case 9:return n.c===r.c;case 7:case 8:case 12:return S(n.b,r.b);case 10:return n.d===r.d&&S(n.b,r.b);case 11:return n.e===r.e&&S(n.b,r.b);case 13:return n.f===r.f&&I(n.g,r.g);case 14:return n.h===r.h&&S(n.b,r.b);case 15:return I(n.g,r.g)}}function I(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;e<t;e++)if(!S(n[e],r[e]))return!1;return!0}function R(n){return n}function D(n){return n}var G=t(function(n,r,t){return t[n]=D(r),t});R(null);function H(n){return{$:0,a:n}}function Z(n){return{$:2,b:n,c:null}}var J=e(function(n,r){return{$:3,b:n,d:r}});var Q=0;function W(n){var r={$:0,e:Q++,f:n,g:null,h:[]};return nn(r),r}function Y(r){return Z(function(n){n(H(W(r)))})}function K(n,r){n.h.push(r),nn(n)}var U=e(function(r,t){return Z(function(n){K(r,t),n(H(v))})});var V=!1,X=[];function nn(n){if(X.push(n),!V){for(V=!0;n=X.shift();)rn(n);V=!1}}function rn(r){for(;r.f;){var n=r.f.$;if(0===n||1===n){for(;r.g&&r.g.$!==n;)r.g=r.g.i;if(!r.g)return;r.f=r.g.b(r.f.a),r.g=r.g.i}else{if(2===n)return void(r.f.c=r.f.b(function(n){r.f=n,nn(r)}));if(5===n){if(0===r.h.length)return;r.f=r.f.b(r.h.shift())}else r.g={$:3===n?0:1,b:r.f.b,i:r.g},r.f=r.f.d}}}function tn(n,r,t,e,u,a){var i=b(q,n,R(r?r.flags:void 0));yr(i)||j(2);var f={},o=(i=t(i.a)).a,c=a(s,o),v=function(n,r){var t;for(var e in en){var u=en[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=an(u,r)}return t}(f,s);function s(n,r){c(o=(i=b(e,n,o)).a,r),vn(f,i.b,u(o))}return vn(f,i.b,u(o)),v?{ports:v}:{}}var en={};function un(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function an(n,r){var e={g:r,h:void 0},u=n.c,a=n.d,i=n.e,f=n.f;function o(t){return b(J,o,{$:5,b:function(n){var r=n.a;return 0===n.$?s(a,e,r,t):i&&f?l(u,e,r.i,r.j,t):s(u,e,i?r.i:r.j,t)}})}return e.h=W(b(J,o,n.b))}var fn=e(function(r,t){return Z(function(n){r.g(t),n(H(v))})}),on=e(function(n,r){return b(U,n.h,{$:0,a:r})});function cn(r){return function(n){return{$:1,k:r,l:n}}}function vn(n,r,t){var e={};for(var u in sn(!0,r,e,null),sn(!1,t,e,null),n)K(n[u],{$:"fx",a:e[u]||{i:$,j:$}})}function sn(n,r,t,e){switch(r.$){case 1:var u=r.k,a=function(n,r,t,e){function u(n){for(var r=t;r;r=r.q)n=r.p(n);return n}return b(n?en[r].e:en[r].f,u,e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:$,j:$},n?t.i=g(r,t.i):t.j=g(r,t.j),t}(n,a,t[u]));case 2:for(var i=r.m;i.b;i=i.b)sn(n,i.a,t,e);return;case 3:return void sn(n,r.o,t,{p:r.n,q:e})}}var bn;var ln="undefined"!=typeof document?document:{};function dn(n,r){n.appendChild(r)}function hn(n){return{$:0,a:n}}var $n=e(function(a,i){return e(function(n,r){for(var t=[],e=0;r.b;r=r.b){var u=r.a;e+=u.b||0,t.push(u)}return e+=t.length,{$:1,c:i,d:kn(n),e:t,f:a,b:e}})})(void 0);e(function(a,i){return e(function(n,r){for(var t=[],e=0;r.b;r=r.b){var u=r.a;e+=u.b.b||0,t.push(u)}return e+=t.length,{$:2,c:i,d:kn(n),e:t,f:a,b:e}})})(void 0);var gn=e(function(n,r){return{$:"a1",n:n,o:r}}),pn=e(function(n,r){return{$:"a2",n:n,o:r}}),mn=e(function(n,r){return{$:"a3",n:n,o:r}});var yn;function kn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,a=t.o;if("a2"!==e){var i=r[e]||(r[e]={});"a3"===e&&"class"===u?wn(i,u,a):i[u]=a}else"className"===u?wn(r,u,D(a)):r[u]=D(a)}return r}function wn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function An(n,r){var t=n.$;if(5===t)return An(n.k||(n.k=n.m()),r);if(0===t)return ln.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var a={j:u,p:r};return(i=An(e,a)).elm_event_node_ref=a,i}if(3===t)return jn(i=n.h(n.g),r,n.d),i;var i=n.f?ln.createElementNS(n.f,n.c):ln.createElement(n.c);bn&&"a"==n.c&&i.addEventListener("click",bn(i)),jn(i,r,n.d);for(var f=n.e,o=0;o<f.length;o++)dn(i,An(1===t?f[o]:f[o].b,r));return i}function jn(n,r,t){for(var e in t){var u=t[e];"a1"===e?_n(n,u):"a0"===e?En(n,r,u):"a3"===e?Tn(n,u):"a4"===e?Nn(n,u):("value"!==e||"checked"!==e||n[e]!==u)&&(n[e]=u)}}function _n(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function Tn(n,r){for(var t in r){var e=r[t];e?n.setAttribute(t,e):n.removeAttribute(t)}}function Nn(n,r){for(var t in r){var e=r[t],u=e.f,a=e.o;a?n.setAttributeNS(u,t,a):n.removeAttributeNS(u,t)}}function En(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var a=t[u],i=e[u];if(a){if(i){if(i.q.$===a.$){i.q=a;continue}n.removeEventListener(u,i)}i=xn(r,a),n.addEventListener(u,i,yn&&{passive:Gt(a)<2}),e[u]=i}else n.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){yn=!0}}))}catch(n){}function xn(v,n){function s(n){var r=s.q,t=O(r.a,n);if(yr(t)){for(var e,u=Gt(r),a=t.a,i=u?u<3?a.a:a.L:a,f=1==u?a.b:3==u&&a.bd,o=(f&&n.stopPropagation(),(2==u?a.b:3==u&&a.a7)&&n.preventDefault(),v);e=o.j;){if("function"==typeof e)i=e(i);else for(var c=e.length;c--;)i=e[c](i);o=o.p}o(i,f)}}return s.q=n,s}function Ln(n,r){return n.$==r.$&&S(n.a,r.a)}function Pn(n,r){var t=[];return Mn(n,r,t,0),t}function Cn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Mn(n,r,t,e){if(n!==r){var u=n.$,a=r.$;if(u!==a){if(1!==u||2!==a)return void Cn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;u<t;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),a=1}switch(a){case 5:for(var i=n.l,f=r.l,o=i.length,c=o===f.length;c&&o--;)c=i[o]===f[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Mn(n.k,r.k,v,0),void(0<v.length&&Cn(t,1,e,v));case 4:for(var s=n.j,b=r.j,l=!1,d=n.k;4===d.$;)l=!0,"object"!=typeof s?s=[s,d.j]:s.push(d.j),d=d.k;for(var h=r.k;4===h.$;)l=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;return l&&s.length!==b.length?void Cn(t,0,e,r):((l?function(n,r){for(var t=0;t<n.length;t++)if(n[t]!==r[t])return!1;return!0}(s,b):s===b)||Cn(t,2,e,b),void Mn(d,h,t,e+1));case 0:return void(n.a!==r.a&&Cn(t,3,e,r.a));case 1:return void qn(n,r,t,e,Fn);case 2:return void qn(n,r,t,e,zn);case 3:if(n.h!==r.h)return void Cn(t,0,e,r);var $=On(n.d,r.d);$&&Cn(t,4,e,$);var g=r.i(n.g,r.g);return void(g&&Cn(t,5,e,g))}}}function qn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var a=On(n.d,r.d);a&&Cn(t,4,e,a),u(n,r,t,e)}else Cn(t,0,e,r)}function On(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var a=n[u],i=r[u];a===i&&"value"!==u&&"checked"!==u||"a0"===t&&Ln(a,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var f=On(n[u],r[u]||{},u);f&&((e=e||{})[u]=f)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function Fn(n,r,t,e){var u=n.e,a=r.e,i=u.length,f=a.length;f<i?Cn(t,6,e,{v:f,i:i-f}):i<f&&Cn(t,7,e,{v:i,e:a});for(var o=i<f?i:f,c=0;c<o;c++){var v=u[c];Mn(v,a[c],t,++e),e+=v.b||0}}function zn(n,r,t,e){for(var u=[],a={},i=[],f=n.e,o=r.e,c=f.length,v=o.length,s=0,b=0,l=e;s<c&&b<v;){var d=(T=f[s]).a,h=(N=o[b]).a,$=T.b,g=N.b;if(d!==h){var p=f[s+1],m=o[b+1];if(p)var y=p.a,k=p.b,w=h===y;if(m)var A=m.a,j=m.b,_=d===A;if(_&&w)Mn($,j,u,++l),Sn(a,u,d,g,b,i),l+=$.b||0,In(a,u,d,k,++l),l+=k.b||0,s+=2,b+=2;else if(_)l++,Sn(a,u,h,g,b,i),Mn($,j,u,l),l+=$.b||0,s+=1,b+=2;else if(w)In(a,u,d,$,++l),l+=$.b||0,Mn(k,g,u,++l),l+=k.b||0,s+=2,b+=1;else{if(!p||y!==A)break;In(a,u,d,$,++l),Sn(a,u,h,g,b,i),l+=$.b||0,Mn(k,j,u,++l),l+=k.b||0,s+=2,b+=2}}else Mn($,g,u,++l),l+=$.b||0,s++,b++}for(;s<c;){var T;In(a,u,(T=f[s]).a,$=T.b,++l),l+=$.b||0,s++}for(;b<v;){var N,E=E||[];Sn(a,u,(N=o[b]).a,N.b,void 0,E),b++}(0<u.length||0<i.length||E)&&Cn(t,8,e,{w:u,x:i,y:E})}var Bn="_elmW6BL";function Sn(n,r,t,e,u,a){var i=n[t];if(!i)return a.push({r:u,A:i={c:0,z:e,r:u,s:void 0}}),void(n[t]=i);if(1===i.c){a.push({r:u,A:i}),i.c=2;var f=[];return Mn(i.z,e,f,i.r),i.r=u,void(i.s.s={w:f,A:i})}Sn(n,r,t+Bn,e,u,a)}function In(n,r,t,e,u){var a=n[t];if(a){if(0===a.c){a.c=2;var i=[];return Mn(e,a.z,i,u),void Cn(r,9,u,{w:i,A:a})}In(n,r,t+Bn,e,u)}else{var f=Cn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:f}}}function Rn(n,r,t,e){!function n(r,t,e,u,a,i,f){var o=e[u];var c=o.r;for(;c===a;){var v=o.$;if(1===v)Rn(r,t.k,o.s,f);else if(8===v){o.t=r,o.u=f;var s=o.s.w;0<s.length&&n(r,t,s,0,a,i,f)}else if(9===v){o.t=r,o.u=f;var b=o.s;if(b){b.A.s=r;var s=b.w;0<s.length&&n(r,t,s,0,a,i,f)}}else o.t=r,o.u=f;if(!(o=e[++u])||(c=o.r)>i)return u}var l=t.$;if(4===l){for(var d=t.k;4===d.$;)d=d.k;return n(r,d,e,u,a+1,i,r.elm_event_node_ref)}var h=t.e;var $=r.childNodes;for(var g=0;g<h.length;g++){var p=1===l?h[g]:h[g].b,m=++a+(p.b||0);if(a<=c&&c<=m&&(u=n($[g],p,e,u,a,m,f),!(o=e[u])||(c=o.r)>i))return u;a=m}return u}(n,r,t,0,0,r.b,e)}function Dn(n,r,t,e){return 0===t.length?n:(Rn(n,r,t,e),Gn(n,t))}function Gn(n,r){for(var t=0;t<r.length;t++){var e=r[t],u=e.t,a=Hn(u,e);u===n&&(n=a)}return n}function Hn(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=An(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return jn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return Gn(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;e<t.i;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,a=n.childNodes[e=t.v];e<u.length;e++)n.insertBefore(An(u[e],r.u),a);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var i=t.A;return void 0!==i.r&&n.parentNode.removeChild(n),i.s=Gn(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=ln.createDocumentFragment(),e=0;e<n.length;e++){var u=n[e],a=u.A;dn(t,2===a.c?a.s:An(a.z,r.u))}return t}(t.y,r);n=Gn(n,t.w);for(var u=t.x,a=0;a<u.length;a++){var i=u[a],f=i.A,o=2===f.c?f.s:An(f.z,r.u);n.insertBefore(o,n.childNodes[i.r])}e&&dn(n,e);return n}(n,r);case 5:return r.s(n);default:j(10)}}function Zn(n){if(3===n.nodeType)return hn(n.textContent);if(1!==n.nodeType)return hn("");for(var r=$,t=n.attributes,e=t.length;e--;){var u=t[e];r=g(b(mn,u.name,u.value),r)}var a=n.tagName.toLowerCase(),i=$,f=n.childNodes;for(e=f.length;e--;)i=g(Zn(f[e]),i);return s($n,a,r,i)}var Jn=u(function(r,n,t,f){return tn(n,f,r.ck,r.cO,r.cI,function(e,n){var u=r.cQ,a=f.node,i=Zn(a);return Wn(n,function(n){var r=u(n),t=Pn(i,r);a=Dn(a,i,t,e),i=r})})}),Qn="undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){setTimeout(n,1e3/60)};function Wn(t,e){e(t);var u=0;function a(){u=1===u?0:(Qn(a),e(t),1)}return function(n,r){t=n,r?(e(t),2===u&&(u=1)):(0===u&&Qn(a),u=2)}}var Yn={addEventListener:function(){},removeEventListener:function(){}};"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var Kn=function(n){return{$:0,a:n}},Un=t(function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,a=b(n,t.a,r);n=u,r=a,t=e}}),Vn=u(function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}}),Xn=f,nr=E,rr=e(function(n,r){return P(r)/P(n)}),tr=nr(b(rr,2,32)),er=[],ur=l(Vn,0,tr,er,er),ar=A,ir=function(n){return s(Un,Xn,$,n)},fr=e(function(n,r){for(;;){var t=b(ar,32,n),e=t.b,u=b(Xn,{$:0,a:t.a},r);if(!e.b)return ir(u);n=e,r=u}}),or=e(function(n,r){for(;;){var t=nr(r/32);if(1===t)return b(ar,32,n).a;n=b(fr,n,$),r=t}}),cr=x,vr=e(function(n,r){return 0<c(n,r)?n:r}),sr=function(n){return n.length},br=e(function(n,r){if(r.k){var t=32*r.k,e=cr(b(rr,32,t-1)),u=n?ir(r.o):r.o,a=b(or,u,r.k);return l(Vn,sr(r.n)+t,b(vr,5,e*tr),a,r.n)}return l(Vn,sr(r.n),tr,er,r.n)}),lr=w,dr=a(function(n,r,t,e,u){for(;;){if(r<0)return b(br,!1,{o:e,k:t/32|0,n:u});var a={$:1,a:s(lr,32,r,n)};n=n,r=r-32,t=t,e=b(Xn,a,e),u=u}}),hr=e(function(n,r){if(0<n){var t=n%32;return d(dr,r,n-t-32,n,$,s(lr,t,n-t,r))}return ur}),$r=function(n){return{$:0,a:n}},gr={$:1},pr=function(n){return{$:1,a:n}},mr=function(n){return{$:0,a:n}},yr=function(n){return!n.$},kr=e(function(n,r){return{$:3,a:n,b:r}}),wr=e(function(n,r){return{$:0,a:n,b:r}}),Ar=e(function(n,r){return{$:1,a:n,b:r}}),jr=function(n){return{$:2,a:n}},_r=t(function(n,r,t){for(;;){if(1<=c(n,r))return t;var e=n,u=r-1,a=b(Xn,r,t);n=e,r=u,t=a}}),Tr=e(function(n,r){return s(_r,n,r,$)}),Nr=M,Er=e(function(n,r){return b(C,n,o(r))}),xr=function(n){return R(s(Un,e(function(n,r){return s(G,n.a,n.b,r)}),{},n))},Lr=R,Pr=e(function(n,r){return xr(p([m("type",Lr("field")),m("name",Lr(n)),m("value",r)]))}),Cr=function(n){return Kn((r=function(n){switch(n){case 0:return"left";case 1:return"right";case 2:return"center";case 3:return"start";default:return"end"}}(n),b(Pr,"textAlign",Lr(r))));var r},Mr=a(function(n,r,t,e,u){return{$:3,a:n,b:r,c:t,d:e,e:u}}),qr=a(function(n,r,t,e,u){return d(Mr,n,r,t,e,u)}),Or=e(function(n,r){return{$:1,a:n,b:r}}),Fr=e(function(n,r){return b(Or,n,r)}),zr=function(n){return{$:1,a:n}},Br=function(n){return{$:2,a:n}},Sr=function(n){return Br(zr(n))},Ir=function(n){var r,t=n.cb;return Kn((r=Nr(n.cG)+"px "+t,b(Pr,"font",Lr(r))))},Rr=function(n){return{$:2,a:n}},Dr=R,Gr=function(n){return Kn(b(Pr,"lineWidth",Dr(n)))},Hr=e(function(n,r){return{$:2,a:n,b:r}}),Zr=e(function(n,r){return b(Hr,n,r)}),Jr=t(function(n,r,t){return{$:0,a:n,b:r,c:t}}),Qr=t(function(n,r,t){return s(Jr,n,r,t)}),Wr=function(n){return{$:0,a:n}},Yr={$:0},Kr=e(function(n,r){return{$:3,a:n,b:r}}),Ur=function(n){return{$:2,a:n}},Vr=e(function(n,r){var t=m(n,r);n:for(;;)switch(t.b.$){case 3:var e=t.b;return b(Kr,e.a,e.b);case 1:switch(t.a.$){case 1:return zr(t.b.a);case 2:return b(Kr,t.b.a,t.a.a);case 3:var u=t.a;return b(Kr,t.b.a,u.b);default:break n}case 2:switch(t.a.$){case 2:return Ur(t.b.a);case 1:return b(Kr,t.a.a,t.b.a);case 3:var a=t.a;return b(Kr,a.a,t.b.a);default:break n}default:if(t.a.$){return t.a}break n}return t.b}),Xr=function(n){return n},nt=e(function(n,r){return s(Un,e(function(n,r){var t=r;switch(n.$){case 0:return y(t,{P:b(Xn,n.a,t.P)});case 1:return y(t,{P:s(Un,Xn,t.P,n.a)});case 3:return y(t,{al:(0,n.a)(t.al)});default:return y(t,{ak:b(Vr,t.ak,n.a)})}}),r,n)}),rt=e(function(n,r){return b(nt,n,{P:$,ak:Yr,al:(t=r,{$:1,a:t})});var t}),tt=function(n){return Br(Ur(n))},et=t(function(n,r,t){return b(nt,n,{P:$,ak:Yr,al:(e={ap:gr,a6:r,aH:t},{$:0,a:e})});var e}),ut=e(function(n,r){return R(s(Un,function(t){return e(function(n,r){return r.push(D(t(n))),r})}(n),[],r))}),at=e(function(n,r){return xr(p([m("type",Lr("function")),m("name",Lr(n)),m("args",b(ut,Xr,r))]))}),it=a(function(n,r,t,e,u){return b(at,"arcTo",p([Dr(n),Dr(r),Dr(t),Dr(e),Dr(u)]))}),ft=i(function(n,r,t,e,u,a){return b(at,"bezierCurveTo",p([Dr(n),Dr(r),Dr(t),Dr(e),Dr(u),Dr(a)]))}),ot=e(function(n,r){return b(at,"lineTo",p([Dr(n),Dr(r)]))}),ct=e(function(n,r){return b(at,"moveTo",p([Dr(n),Dr(r)]))}),vt=u(function(n,r,t,e){return b(at,"quadraticCurveTo",p([Dr(n),Dr(r),Dr(t),Dr(e)]))}),st=e(function(n,r){switch(n.$){case 0:var t=n.a,e=n.b;return b(Xn,d(it,t.a,t.b,e.a,e.b,n.c),r);case 1:var u=n.a,a=n.b,i=n.c;return b(Xn,h(ft,u.a,u.b,a.a,a.b,i.a,i.b),r);case 2:var f=n.a;return b(Xn,b(ot,f.a,f.b),r);case 3:var o=n.a;return b(Xn,b(ct,o.a,o.b),r);default:var c=n.a,v=n.b;return b(Xn,l(vt,c.a,c.b,v.a,v.b),r)}}),bt=R,lt=i(function(n,r,t,e,u,a){return b(at,"arc",p([Dr(n),Dr(r),Dr(t),Dr(e),Dr(u),bt(a)]))}),dt=t(function(n,r,t){return h(lt,n,r,t,0,6.283185307179586,!1)}),ht=u(function(n,r,t,e){return b(at,"rect",p([Dr(n),Dr(r),Dr(t),Dr(e)]))}),$t=T,gt=N,pt=e(function(n,r){switch(n.$){case 0:var t=n.a;return b(Xn,l(ht,f=t.a,o=t.b,n.b,n.c),b(Xn,b(ct,f,o),r));case 1:var e=n.a,u=n.b;return b(Xn,s(dt,f=e.a,o=e.b,u),b(Xn,b(ct,f+u,o),r));case 2:var a=n.a,i=n.b;return s(Un,st,b(Xn,b(ct,f=a.a,o=a.b),r),i);default:var f,o,c=n.a,v=n.c;return b(Xn,h(lt,f=c.a,o=c.b,n.b,v,n.d,n.e),b(Xn,b(ct,f+$t(v),o+gt(v)),r))}}),mt=L,yt=M,kt=function(n){var r,t,e=n.b,u=n.c,a=n.d,i=function(n){return mt(1e4*n)/100};return r=p(["rgba(",yt(i(n.a)),"%,",yt(i(e)),"%,",yt(i(u)),"%,",yt((t=a,mt(1e3*t)/1e3)),")"]),b(Er,"",r)},wt=function(n){return b(Pr,"fillStyle",Lr(kt(n)))},At=e(function(n,r){return b(Xn,b(at,"fill",p([Lr(function(n){return n?"evenodd":"nonzero"}(0))])),b(Xn,wt(n),r))}),jt=b(at,"stroke",$),_t=function(n){return b(Pr,"strokeStyle",Lr(kt(n)))},Tt=e(function(n,r){return b(Xn,jt,b(Xn,_t(n),r))}),Nt=u(function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}}),Et=l(Nt,0,0,0,1),xt=e(function(n,r){switch(n.$){case 0:return b(At,Et,r);case 1:return b(At,n.a,r);case 2:return b(Tt,n.a,r);default:return b(Tt,n.b,b(At,n.a,r))}}),Lt=u(function(n,r,t,e){if(1===e.$)return b(at,"fillText",p([Lr(n),Dr(r),Dr(t)]));var u=e.a;return b(at,"fillText",p([Lr(n),Dr(r),Dr(t),Dr(u)]))}),Pt=a(function(n,r,t,e,u){return b(Xn,l(Lt,n.aH,r,t,n.ap),b(Xn,wt(e),u))}),Ct=u(function(n,r,t,e){if(1===e.$)return b(at,"strokeText",p([Lr(n),Dr(r),Dr(t)]));var u=e.a;return b(at,"strokeText",p([Lr(n),Dr(r),Dr(t),Dr(u)]))}),Mt=a(function(n,r,t,e,u){return b(Xn,l(Ct,n.aH,r,t,n.ap),b(Xn,_t(e),u))}),qt=t(function(n,r,t){var e=r.a6,u=e.a,a=e.b;switch(n.$){case 0:return d(Pt,r,u,a,Et,t);case 1:return d(Pt,r,u,a,n.a,t);case 2:return d(Mt,r,u,a,n.a,t);default:return d(Mt,r,u,a,n.b,d(Pt,r,u,a,n.a,t))}}),Ot=t(function(n,r,t){return s(qt,n,r,t)}),Ft=b(at,"beginPath",$),zt=t(function(n,r,t){if(n.$){var e=n.a;return b(xt,r,s(Un,pt,b(Xn,Ft,t),e))}return s(Ot,r,n.a,t)}),Bt=b(at,"restore",$),St=b(at,"save",$),It=e(function(n,r){return b(Xn,Bt,s(zt,n.al,n.ak,k(n.P,b(Xn,St,r))))}),Rt=$,Dt=function(n){return{$:0,a:n}},Gt=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Ht=e(function(n,r){return b(pn,function(n){return"innerHTML"==n||"formAction"==n?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(r))}),Zt=$n("canvas"),Jt=function(n){return $n(function(n){return"script"==n?"p":n}(n))},Qt=t(function(n,r,t){var e,u,a=n.a,i=n.b;return s(Jt,"elm-canvas",p([(u=function(n){return s(Un,It,Rt,n)}(t),b(Ht,"cmds",b(ut,Xr,u)))]),p([b(Zt,b(Xn,(e=i,b(mn,"height",Nr(e))),b(Xn,function(n){return b(mn,"width",Nr(n))}(a),r)),$)]))}),Wt=e(function(n,r){return b(at,"scale",p([Dr(n),Dr(r)]))}),Yt=i(function(n,r,t,e,u,a){return b(at,"transform",p([Dr(n),Dr(r),Dr(t),Dr(e),Dr(u),Dr(a)]))}),Kt=e(function(n,r){return b(at,"translate",p([Dr(n),Dr(r)]))}),Ut=u(function(n,r,t,e){if(e.b){var u=e.a,a=e.b;if(a.b){var i=a.a,f=a.b;if(f.b){var o=f.a,c=f.b;if(c.b){var v=c.b;return b(n,u,b(n,i,b(n,o,b(n,c.a,500<t?s(Un,n,r,ir(v)):l(Ut,n,r,t+1,v)))))}return b(n,u,b(n,i,b(n,o,r)))}return b(n,u,b(n,i,r))}return b(n,u,r)}return r}),Vt=t(function(n,r,t){return l(Ut,n,r,0,t)}),Xt=e(function(t,n){return s(Vt,e(function(n,r){return b(Xn,t(n),r)}),$,n)}),ne=function(n){return{$:1,a:b(Xt,function(n){switch(n.$){case 0:return function(n){return b(at,"rotate",p([Dr(n)]))}(n.a);case 1:return b(Wt,n.a,n.b);case 2:return b(Kt,n.a,n.b);default:return h(Yt,n.a,n.b,n.c,n.d,n.e,n.f)}},n)}},re=e(function(n,r){return{$:2,a:n,b:r}}),te=l(Nt,52/255,101/255,164/255,1),ee=l(Nt,115/255,210/255,22/255,1),ue=l(Nt,173/255,127/255,168/255,1),ae=l(Nt,.8,0,0,1),ie=l(Nt,1,1,1,1),fe=function(n){return 3.141592653589793*n/180},oe=_,ce=gn,ve=H,se=ve(0),be=J,le=e(function(r,n){return b(be,function(n){return ve(r(n))},n)}),de=t(function(t,n,e){return b(be,function(r){return b(be,function(n){return ve(b(t,r,n))},e)},n)}),he=function(n){return s(Vt,de(Xn),ve($),n)},$e=fn,ge=e(function(n,r){var t=r;return Y(b(be,$e(n),t))});en.Task=un(se,t(function(n,r){return b(le,function(){return 0},he(b(Xt,ge(n),r)))}),t(function(){return ve(0)}),e(function(n,r){return b(le,n,r)}));cn("Task");var pe=Jn,me=function(n){return{$:1,a:n}},ye=t(function(n,r,t){return{aZ:t,bM:r,bP:n}}),ke=ve(s(ye,$,gr,0)),we=Z(function(n){n(H(Date.now()))}),Ae=Z(function(n){var r=requestAnimationFrame(function(){n(H(Date.now()))});return function(){cancelAnimationFrame(r)}}),je=on,_e=function(t){return Z(function(n){var r=t.f;2===r.$&&r.c&&r.c(),t.f=null,n(H(v))})},Te=Y,Ne=t(function(n,t,r){var e=r.bM,u=r.aZ,a=m(e,t);if(1!==a.a.$)return a.b.b?ve(s(ye,t,e,u)):b(be,function(){return ke},_e(a.a.a));if(a.b.b){return b(be,function(r){return b(be,function(n){return ve(s(ye,t,$r(r),n))},we)},Te(b(be,je(n),Ae)))}return ke}),Ee=Xr,xe=t(function(r,t,n){var e=n.bP,u=n.aZ,a=function(n){return b($e,r,n.$?(0,n.a)(t-u):(0,n.a)(Ee(t)))};return b(be,function(n){return b(be,function(){return ve(s(ye,e,$r(n),t))},he(b(Xt,a,e)))},Te(b(be,je(r),Ae)))}),Le=t(function(n,r,t){return n(r(t))});en["Browser.AnimationManager"]=un(ke,Ne,xe,0,e(function(n,r){return r.$?me(b(Le,n,r.a)):{$:0,a:b(Le,n,r.a)}}));var Pe,Ce=cn("Browser.AnimationManager"),Me=function(n){return Ce(me(n))},qe=function(n){return{$:2,m:n}}($),Oe=pe({ck:function(){return m(m(0,0),qe)},cI:function(){return Me(Xr)},cO:e(function(n,r){return m(m(r.a+1,1e3/n),qe)}),cQ:function(n){var r=n.a,t=n.b;return s(Qt,m(500,500),p([b(ce,"border","2px solid red")]),p([b(rt,p([Sr(ie)]),p([s(Qr,m(0,0),500,500)])),b(rt,p([Gr(5),ne(p([b(re,250,250),Wr(fe(360*gt(r/100)))])),Sr(ae),tt(ee)]),p([s(Qr,m(-100,-150),40,50),b(Fr,m(100,100),80)])),s(et,p([Cr(1),Ir({cb:"sans-serif",cG:30}),Gr(1),tt(te),Sr(ee)]),m(450,50),"fps: "+Nr(cr(t))),b(rt,$,p([b(Zr,m(20,10),p([Rr(m(10,30)),Rr(m(30,30)),Rr(m(20,10))])),b(Fr,m(50,50),10)])),b(rt,p([Sr(ue),ne(p([b(re,10,400)]))]),b(Xt,function(n){return d(qr,m(40*n+40,0),20,fe(-45),fe(45),!!b(oe,2,n))},b(Tr,0,10)))]))}});Pe={Examples:{NewApiTest:{init:Oe(Dt(0))(0)}}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?j(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,Pe):n.Elm=Pe}(this);