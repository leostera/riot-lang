open Std

module HighlightJs = struct
  let source = {|
/*!
  Highlight.js v11.11.1 (git: 08cb242e7d)
  (c) 2006-2025 Josh Goebel <hello@joshgoebel.com> and other contributors
  License: BSD-3-Clause
 */
var hljs=function(){"use strict";function e(t){
return t instanceof Map?t.clear=t.delete=t.set=()=>{
throw Error("map is read-only")}:t instanceof Set&&(t.add=t.clear=t.delete=()=>{
throw Error("set is read-only")
}),Object.freeze(t),Object.getOwnPropertyNames(t).forEach((n=>{
const i=t[n],s=typeof i;"object"!==s&&"function"!==s||Object.isFrozen(i)||e(i)
})),t}class t{constructor(e){
void 0===e.data&&(e.data={}),this.data=e.data,this.isMatchIgnored=!1}
ignoreMatch(){this.isMatchIgnored=!0}}function n(e){
return e.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#x27;")
}function i(e,...t){const n=Object.create(null);for(const t in e)n[t]=e[t]
;return t.forEach((e=>{for(const t in e)n[t]=e[t]})),n}const s=e=>!!e.scope
;class r{constructor(e,t){
this.buffer="",this.classPrefix=t.classPrefix,e.walk(this)}addText(e){
this.buffer+=n(e)}openNode(e){if(!s(e))return;const t=((e,{prefix:t})=>{
if(e.startsWith("language:"))return e.replace("language:","language-")
;if(e.includes(".")){const n=e.split(".")
;return[`${t}${n.shift()}`,...n.map(((e,t)=>`${e}${"_".repeat(t+1)}`))].join(" ")
}return`${t}${e}`})(e.scope,{prefix:this.classPrefix});this.span(t)}
closeNode(e){s(e)&&(this.buffer+="</span>")}value(){return this.buffer}span(e){
this.buffer+=`<span class="${e}">`}}const o=(e={})=>{const t={children:[]}
;return Object.assign(t,e),t};class a{constructor(){
this.rootNode=o(),this.stack=[this.rootNode]}get top(){
return this.stack[this.stack.length-1]}get root(){return this.rootNode}add(e){
this.top.children.push(e)}openNode(e){const t=o({scope:e})
;this.add(t),this.stack.push(t)}closeNode(){
if(this.stack.length>1)return this.stack.pop()}closeAllNodes(){
for(;this.closeNode(););}toJSON(){return JSON.stringify(this.rootNode,null,4)}
walk(e){return this.constructor._walk(e,this.rootNode)}static _walk(e,t){
return"string"==typeof t?e.addText(t):t.children&&(e.openNode(t),
t.children.forEach((t=>this._walk(e,t))),e.closeNode(t)),e}static _collapse(e){
"string"!=typeof e&&e.children&&(e.children.every((e=>"string"==typeof e))?e.children=[e.children.join("")]:e.children.forEach((e=>{
a._collapse(e)})))}}class c extends a{constructor(e){super(),this.options=e}
addText(e){""!==e&&this.add(e)}startScope(e){this.openNode(e)}endScope(){
this.closeNode()}__addSublanguage(e,t){const n=e.root
;t&&(n.scope="language:"+t),this.add(n)}toHTML(){
return new r(this,this.options).value()}finalize(){
return this.closeAllNodes(),!0}}function l(e){
return e?"string"==typeof e?e:e.source:null}function g(e){return h("(?=",e,")")}
function u(e){return h("(?:",e,")*")}function d(e){return h("(?:",e,")?")}
function h(...e){return e.map((e=>l(e))).join("")}function f(...e){const t=(e=>{
const t=e[e.length-1]
;return"object"==typeof t&&t.constructor===Object?(e.splice(e.length-1,1),t):{}
})(e);return"("+(t.capture?"":"?:")+e.map((e=>l(e))).join("|")+")"}
function p(e){return RegExp(e.toString()+"|").exec("").length-1}
const b=/\[(?:[^\\\]]|\\.)*\]|\(\??|\\([1-9][0-9]*)|\\./
;function m(e,{joinWith:t}){let n=0;return e.map((e=>{n+=1;const t=n
;let i=l(e),s="";for(;i.length>0;){const e=b.exec(i);if(!e){s+=i;break}
s+=i.substring(0,e.index),
i=i.substring(e.index+e[0].length),"\\"===e[0][0]&&e[1]?s+="\\"+(Number(e[1])+t):(s+=e[0],
"("===e[0]&&n++)}return s})).map((e=>`(${e})`)).join(t)}
const E="[a-zA-Z]\\w*",x="[a-zA-Z_]\\w*",y="\\b\\d+(\\.\\d+)?",_="(-?)(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)",w="\\b(0b[01]+)",O={
begin:"\\\\[\\s\\S]",relevance:0},v={scope:"string",begin:"'",end:"'",
illegal:"\\n",contains:[O]},k={scope:"string",begin:'"',end:'"',illegal:"\\n",
contains:[O]},N=(e,t,n={})=>{const s=i({scope:"comment",begin:e,end:t,
contains:[]},n);s.contains.push({scope:"doctag",
begin:"[ ]*(?=(TODO|FIXME|NOTE|BUG|OPTIMIZE|HACK|XXX):)",
end:/(TODO|FIXME|NOTE|BUG|OPTIMIZE|HACK|XXX):/,excludeBegin:!0,relevance:0})
;const r=f("I","a","is","so","us","to","at","if","in","it","on",/[A-Za-z]+['](d|ve|re|ll|t|s|n)/,/[A-Za-z]+[-][a-z]+/,/[A-Za-z][a-z]{2,}/)
;return s.contains.push({begin:h(/[ ]+/,"(",r,/[.]?[:]?([.][ ]|[ ])/,"){3}")}),s
},S=N("//","$"),M=N("/\\*","\\*/"),R=N("#","$");var j=Object.freeze({
__proto__:null,APOS_STRING_MODE:v,BACKSLASH_ESCAPE:O,BINARY_NUMBER_MODE:{
scope:"number",begin:w,relevance:0},BINARY_NUMBER_RE:w,COMMENT:N,
C_BLOCK_COMMENT_MODE:M,C_LINE_COMMENT_MODE:S,C_NUMBER_MODE:{scope:"number",
begin:_,relevance:0},C_NUMBER_RE:_,END_SAME_AS_BEGIN:e=>Object.assign(e,{
"on:begin":(e,t)=>{t.data._beginMatch=e[1]},"on:end":(e,t)=>{
t.data._beginMatch!==e[1]&&t.ignoreMatch()}}),HASH_COMMENT_MODE:R,IDENT_RE:E,
MATCH_NOTHING_RE:/\b\B/,METHOD_GUARD:{begin:"\\.\\s*"+x,relevance:0},
NUMBER_MODE:{scope:"number",begin:y,relevance:0},NUMBER_RE:y,
PHRASAL_WORDS_MODE:{
begin:/\b(a|an|the|are|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|they|like|more)\b/
},QUOTE_STRING_MODE:k,REGEXP_MODE:{scope:"regexp",begin:/\/(?=[^/\n]*\/)/,
end:/\/[gimuy]*/,contains:[O,{begin:/\[/,end:/\]/,relevance:0,contains:[O]}]},
RE_STARTERS_RE:"!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~",
SHEBANG:(e={})=>{const t=/^#![ ]*\//
;return e.binary&&(e.begin=h(t,/.*\b/,e.binary,/\b.*/)),i({scope:"meta",begin:t,
end:/$/,relevance:0,"on:begin":(e,t)=>{0!==e.index&&t.ignoreMatch()}},e)},
TITLE_MODE:{scope:"title",begin:E,relevance:0},UNDERSCORE_IDENT_RE:x,
UNDERSCORE_TITLE_MODE:{scope:"title",begin:x,relevance:0}});function A(e,t){
"."===e.input[e.index-1]&&t.ignoreMatch()}function I(e,t){
void 0!==e.className&&(e.scope=e.className,delete e.className)}function T(e,t){
t&&e.beginKeywords&&(e.begin="\\b("+e.beginKeywords.split(" ").join("|")+")(?!\\.)(?=\\b|\\s)",
e.__beforeBegin=A,e.keywords=e.keywords||e.beginKeywords,delete e.beginKeywords,
void 0===e.relevance&&(e.relevance=0))}function L(e,t){
Array.isArray(e.illegal)&&(e.illegal=f(...e.illegal))}function B(e,t){
if(e.match){
if(e.begin||e.end)throw Error("begin & end are not supported with match")
;e.begin=e.match,delete e.match}}function P(e,t){
void 0===e.relevance&&(e.relevance=1)}const D=(e,t)=>{if(!e.beforeMatch)return
;if(e.starts)throw Error("beforeMatch cannot be used with starts")
;const n=Object.assign({},e);Object.keys(e).forEach((t=>{delete e[t]
})),e.keywords=n.keywords,e.begin=h(n.beforeMatch,g(n.begin)),e.starts={
relevance:0,contains:[Object.assign(n,{endsParent:!0})]
},e.relevance=0,delete n.beforeMatch
},H=["of","and","for","in","not","or","if","then","parent","list","value"]
;function C(e,t,n="keyword"){const i=Object.create(null)
;return"string"==typeof e?s(n,e.split(" ")):Array.isArray(e)?s(n,e):Object.keys(e).forEach((n=>{
Object.assign(i,C(e[n],t,n))})),i;function s(e,n){
t&&(n=n.map((e=>e.toLowerCase()))),n.forEach((t=>{const n=t.split("|")
;i[n[0]]=[e,$(n[0],n[1])]}))}}function $(e,t){
return t?Number(t):(e=>H.includes(e.toLowerCase()))(e)?0:1}const U={},z=e=>{
console.error(e)},W=(e,...t)=>{console.log("WARN: "+e,...t)},X=(e,t)=>{
U[`${e}/${t}`]||(console.log(`Deprecated as of ${e}. ${t}`),U[`${e}/${t}`]=!0)
},G=Error();function K(e,t,{key:n}){let i=0;const s=e[n],r={},o={}
;for(let e=1;e<=t.length;e++)o[e+i]=s[e],r[e+i]=!0,i+=p(t[e-1])
;e[n]=o,e[n]._emit=r,e[n]._multi=!0}function F(e){(e=>{
e.scope&&"object"==typeof e.scope&&null!==e.scope&&(e.beginScope=e.scope,
delete e.scope)})(e),"string"==typeof e.beginScope&&(e.beginScope={
_wrap:e.beginScope}),"string"==typeof e.endScope&&(e.endScope={_wrap:e.endScope
}),(e=>{if(Array.isArray(e.begin)){
if(e.skip||e.excludeBegin||e.returnBegin)throw z("skip, excludeBegin, returnBegin not compatible with beginScope: {}"),
G
;if("object"!=typeof e.beginScope||null===e.beginScope)throw z("beginScope must be object"),
G;K(e,e.begin,{key:"beginScope"}),e.begin=m(e.begin,{joinWith:""})}})(e),(e=>{
if(Array.isArray(e.end)){
if(e.skip||e.excludeEnd||e.returnEnd)throw z("skip, excludeEnd, returnEnd not compatible with endScope: {}"),
G
;if("object"!=typeof e.endScope||null===e.endScope)throw z("endScope must be object"),
G;K(e,e.end,{key:"endScope"}),e.end=m(e.end,{joinWith:""})}})(e)}function Z(e){
function t(t,n){
return RegExp(l(t),"m"+(e.case_insensitive?"i":"")+(e.unicodeRegex?"u":"")+(n?"g":""))
}class n{constructor(){
this.matchIndexes={},this.regexes=[],this.matchAt=1,this.position=0}
addRule(e,t){
t.position=this.position++,this.matchIndexes[this.matchAt]=t,this.regexes.push([t,e]),
this.matchAt+=p(e)+1}compile(){0===this.regexes.length&&(this.exec=()=>null)
;const e=this.regexes.map((e=>e[1]));this.matcherRe=t(m(e,{joinWith:"|"
}),!0),this.lastIndex=0}exec(e){this.matcherRe.lastIndex=this.lastIndex
;const t=this.matcherRe.exec(e);if(!t)return null
;const n=t.findIndex(((e,t)=>t>0&&void 0!==e)),i=this.matchIndexes[n]
;return t.splice(0,n),Object.assign(t,i)}}class s{constructor(){
this.rules=[],this.multiRegexes=[],
this.count=0,this.lastIndex=0,this.regexIndex=0}getMatcher(e){
if(this.multiRegexes[e])return this.multiRegexes[e];const t=new n
;return this.rules.slice(e).forEach((([e,n])=>t.addRule(e,n))),
t.compile(),this.multiRegexes[e]=t,t}resumingScanAtSamePosition(){
return 0!==this.regexIndex}considerAll(){this.regexIndex=0}addRule(e,t){
this.rules.push([e,t]),"begin"===t.type&&this.count++}exec(e){
const t=this.getMatcher(this.regexIndex);t.lastIndex=this.lastIndex
;let n=t.exec(e)
;if(this.resumingScanAtSamePosition())if(n&&n.index===this.lastIndex);else{
const t=this.getMatcher(0);t.lastIndex=this.lastIndex+1,n=t.exec(e)}
return n&&(this.regexIndex+=n.position+1,
this.regexIndex===this.count&&this.considerAll()),n}}
if(e.compilerExtensions||(e.compilerExtensions=[]),
e.contains&&e.contains.includes("self"))throw Error("ERR: contains `self` is not supported at the top-level of a language.  See documentation.")
;return e.classNameAliases=i(e.classNameAliases||{}),function n(r,o){const a=r
;if(r.isCompiled)return a
;[I,B,F,D].forEach((e=>e(r,o))),e.compilerExtensions.forEach((e=>e(r,o))),
r.__beforeBegin=null,[T,L,P].forEach((e=>e(r,o))),r.isCompiled=!0;let c=null
;return"object"==typeof r.keywords&&r.keywords.$pattern&&(r.keywords=Object.assign({},r.keywords),
c=r.keywords.$pattern,
delete r.keywords.$pattern),c=c||/\w+/,r.keywords&&(r.keywords=C(r.keywords,e.case_insensitive)),
a.keywordPatternRe=t(c,!0),
o&&(r.begin||(r.begin=/\B|\b/),a.beginRe=t(a.begin),r.end||r.endsWithParent||(r.end=/\B|\b/),
r.end&&(a.endRe=t(a.end)),
a.terminatorEnd=l(a.end)||"",r.endsWithParent&&o.terminatorEnd&&(a.terminatorEnd+=(r.end?"|":"")+o.terminatorEnd)),
r.illegal&&(a.illegalRe=t(r.illegal)),
r.contains||(r.contains=[]),r.contains=[].concat(...r.contains.map((e=>(e=>(e.variants&&!e.cachedVariants&&(e.cachedVariants=e.variants.map((t=>i(e,{
variants:null},t)))),e.cachedVariants?e.cachedVariants:V(e)?i(e,{
starts:e.starts?i(e.starts):null
}):Object.isFrozen(e)?i(e):e))("self"===e?r:e)))),r.contains.forEach((e=>{n(e,a)
})),r.starts&&n(r.starts,o),a.matcher=(e=>{const t=new s
;return e.contains.forEach((e=>t.addRule(e.begin,{rule:e,type:"begin"
}))),e.terminatorEnd&&t.addRule(e.terminatorEnd,{type:"end"
}),e.illegal&&t.addRule(e.illegal,{type:"illegal"}),t})(a),a}(e)}function V(e){
return!!e&&(e.endsWithParent||V(e.starts))}class q extends Error{
constructor(e,t){super(e),this.name="HTMLInjectionError",this.html=t}}
const J=n,Y=i,Q=Symbol("nomatch"),ee=n=>{
const i=Object.create(null),s=Object.create(null),r=[];let o=!0
;const a="Could not find the language '{}', did you forget to load/include a language module?",l={
disableAutodetect:!0,name:"Plain text",contains:[]};let p={
ignoreUnescapedHTML:!1,throwUnescapedHTML:!1,noHighlightRe:/^(no-?highlight)$/i,
languageDetectRe:/\blang(?:uage)?-([\w-]+)\b/i,classPrefix:"hljs-",
cssSelector:"pre code",languages:null,__emitter:c};function b(e){
return p.noHighlightRe.test(e)}function m(e,t,n){let i="",s=""
;"object"==typeof t?(i=e,
n=t.ignoreIllegals,s=t.language):(X("10.7.0","highlight(lang, code, ...args) has been deprecated."),
X("10.7.0","Please use highlight(code, options) instead.\nhttps://github.com/highlightjs/highlight.js/issues/2277"),
s=e,i=t),void 0===n&&(n=!0);const r={code:i,language:s};N("before:highlight",r)
;const o=r.result?r.result:E(r.language,r.code,n)
;return o.code=r.code,N("after:highlight",o),o}function E(e,n,s,r){
const c=Object.create(null);function l(){if(!N.keywords)return void M.addText(R)
;let e=0;N.keywordPatternRe.lastIndex=0;let t=N.keywordPatternRe.exec(R),n=""
;for(;t;){n+=R.substring(e,t.index)
;const s=w.case_insensitive?t[0].toLowerCase():t[0],r=(i=s,N.keywords[i]);if(r){
const[e,i]=r
;if(M.addText(n),n="",c[s]=(c[s]||0)+1,c[s]<=7&&(j+=i),e.startsWith("_"))n+=t[0];else{
const n=w.classNameAliases[e]||e;u(t[0],n)}}else n+=t[0]
;e=N.keywordPatternRe.lastIndex,t=N.keywordPatternRe.exec(R)}var i
;n+=R.substring(e),M.addText(n)}function g(){null!=N.subLanguage?(()=>{
if(""===R)return;let e=null;if("string"==typeof N.subLanguage){
if(!i[N.subLanguage])return void M.addText(R)
;e=E(N.subLanguage,R,!0,S[N.subLanguage]),S[N.subLanguage]=e._top
}else e=x(R,N.subLanguage.length?N.subLanguage:null)
;N.relevance>0&&(j+=e.relevance),M.__addSublanguage(e._emitter,e.language)
})():l(),R=""}function u(e,t){
""!==e&&(M.startScope(t),M.addText(e),M.endScope())}function d(e,t){let n=1
;const i=t.length-1;for(;n<=i;){if(!e._emit[n]){n++;continue}
const i=w.classNameAliases[e[n]]||e[n],s=t[n];i?u(s,i):(R=s,l(),R=""),n++}}
function h(e,t){
return e.scope&&"string"==typeof e.scope&&M.openNode(w.classNameAliases[e.scope]||e.scope),
e.beginScope&&(e.beginScope._wrap?(u(R,w.classNameAliases[e.beginScope._wrap]||e.beginScope._wrap),
R=""):e.beginScope._multi&&(d(e.beginScope,t),R="")),N=Object.create(e,{parent:{
value:N}}),N}function f(e,n,i){let s=((e,t)=>{const n=e&&e.exec(t)
;return n&&0===n.index})(e.endRe,i);if(s){if(e["on:end"]){const i=new t(e)
;e["on:end"](n,i),i.isMatchIgnored&&(s=!1)}if(s){
for(;e.endsParent&&e.parent;)e=e.parent;return e}}
if(e.endsWithParent)return f(e.parent,n,i)}function b(e){
return 0===N.matcher.regexIndex?(R+=e[0],1):(T=!0,0)}function m(e){
const t=e[0],i=n.substring(e.index),s=f(N,e,i);if(!s)return Q;const r=N
;N.endScope&&N.endScope._wrap?(g(),
u(t,N.endScope._wrap)):N.endScope&&N.endScope._multi?(g(),
d(N.endScope,e)):r.skip?R+=t:(r.returnEnd||r.excludeEnd||(R+=t),
g(),r.excludeEnd&&(R=t));do{
N.scope&&M.closeNode(),N.skip||N.subLanguage||(j+=N.relevance),N=N.parent
}while(N!==s.parent);return s.starts&&h(s.starts,e),r.returnEnd?0:t.length}
let y={};function _(i,r){const a=r&&r[0];if(R+=i,null==a)return g(),0
;if("begin"===y.type&&"end"===r.type&&y.index===r.index&&""===a){
if(R+=n.slice(r.index,r.index+1),!o){const t=Error(`0 width match regex (${e})`)
;throw t.languageName=e,t.badRule=y.rule,t}return 1}
if(y=r,"begin"===r.type)return(e=>{
const n=e[0],i=e.rule,s=new t(i),r=[i.__beforeBegin,i["on:begin"]]
;for(const t of r)if(t&&(t(e,s),s.isMatchIgnored))return b(n)
;return i.skip?R+=n:(i.excludeBegin&&(R+=n),
g(),i.returnBegin||i.excludeBegin||(R=n)),h(i,e),i.returnBegin?0:n.length})(r)
;if("illegal"===r.type&&!s){
const e=Error('Illegal lexeme "'+a+'" for mode "'+(N.scope||"<unnamed>")+'"')
;throw e.mode=N,e}if("end"===r.type){const e=m(r);if(e!==Q)return e}
if("illegal"===r.type&&""===a)return R+="\n",1
;if(I>1e5&&I>3*r.index)throw Error("potential infinite loop, way more iterations than matches")
;return R+=a,a.length}const w=O(e)
;if(!w)throw z(a.replace("{}",e)),Error('Unknown language: "'+e+'"')
;const v=Z(w);let k="",N=r||v;const S={},M=new p.__emitter(p);(()=>{const e=[]
;for(let t=N;t!==w;t=t.parent)t.scope&&e.unshift(t.scope)
;e.forEach((e=>M.openNode(e)))})();let R="",j=0,A=0,I=0,T=!1;try{
if(w.__emitTokens)w.__emitTokens(n,M);else{for(N.matcher.considerAll();;){
I++,T?T=!1:N.matcher.considerAll(),N.matcher.lastIndex=A
;const e=N.matcher.exec(n);if(!e)break;const t=_(n.substring(A,e.index),e)
;A=e.index+t}_(n.substring(A))}return M.finalize(),k=M.toHTML(),{language:e,
value:k,relevance:j,illegal:!1,_emitter:M,_top:N}}catch(t){
if(t.message&&t.message.includes("Illegal"))return{language:e,value:J(n),
illegal:!0,relevance:0,_illegalBy:{message:t.message,index:A,
context:n.slice(A-100,A+100),mode:t.mode,resultSoFar:k},_emitter:M};if(o)return{
language:e,value:J(n),illegal:!1,relevance:0,errorRaised:t,_emitter:M,_top:N}
;throw t}}function x(e,t){t=t||p.languages||Object.keys(i);const n=(e=>{
const t={value:J(e),illegal:!1,relevance:0,_top:l,_emitter:new p.__emitter(p)}
;return t._emitter.addText(e),t})(e),s=t.filter(O).filter(k).map((t=>E(t,e,!1)))
;s.unshift(n);const r=s.sort(((e,t)=>{
if(e.relevance!==t.relevance)return t.relevance-e.relevance
;if(e.language&&t.language){if(O(e.language).supersetOf===t.language)return 1
;if(O(t.language).supersetOf===e.language)return-1}return 0})),[o,a]=r,c=o
;return c.secondBest=a,c}function y(e){let t=null;const n=(e=>{
let t=e.className+" ";t+=e.parentNode?e.parentNode.className:""
;const n=p.languageDetectRe.exec(t);if(n){const t=O(n[1])
;return t||(W(a.replace("{}",n[1])),
W("Falling back to no-highlight mode for this block.",e)),t?n[1]:"no-highlight"}
return t.split(/\s+/).find((e=>b(e)||O(e)))})(e);if(b(n))return
;if(N("before:highlightElement",{el:e,language:n
}),e.dataset.highlighted)return void console.log("Element previously highlighted. To highlight again, first unset `dataset.highlighted`.",e)
;if(e.children.length>0&&(p.ignoreUnescapedHTML||(console.warn("One of your code blocks includes unescaped HTML. This is a potentially serious security risk."),
console.warn("https://github.com/highlightjs/highlight.js/wiki/security"),
console.warn("The element with unescaped HTML:"),
console.warn(e)),p.throwUnescapedHTML))throw new q("One of your code blocks includes unescaped HTML.",e.innerHTML)
;t=e;const i=t.textContent,r=n?m(i,{language:n,ignoreIllegals:!0}):x(i)
;e.innerHTML=r.value,e.dataset.highlighted="yes",((e,t,n)=>{const i=t&&s[t]||n
;e.classList.add("hljs"),e.classList.add("language-"+i)
})(e,n,r.language),e.result={language:r.language,re:r.relevance,
relevance:r.relevance},r.secondBest&&(e.secondBest={
language:r.secondBest.language,relevance:r.secondBest.relevance
}),N("after:highlightElement",{el:e,result:r,text:i})}let _=!1;function w(){
if("loading"===document.readyState)return _||window.addEventListener("DOMContentLoaded",(()=>{
w()}),!1),void(_=!0);document.querySelectorAll(p.cssSelector).forEach(y)}
function O(e){return e=(e||"").toLowerCase(),i[e]||i[s[e]]}
function v(e,{languageName:t}){"string"==typeof e&&(e=[e]),e.forEach((e=>{
s[e.toLowerCase()]=t}))}function k(e){const t=O(e)
;return t&&!t.disableAutodetect}function N(e,t){const n=e;r.forEach((e=>{
e[n]&&e[n](t)}))}Object.assign(n,{highlight:m,highlightAuto:x,highlightAll:w,
highlightElement:y,
highlightBlock:e=>(X("10.7.0","highlightBlock will be removed entirely in v12.0"),
X("10.7.0","Please use highlightElement now."),y(e)),configure:e=>{p=Y(p,e)},
initHighlighting:()=>{
w(),X("10.6.0","initHighlighting() deprecated.  Use highlightAll() now.")},
initHighlightingOnLoad:()=>{
w(),X("10.6.0","initHighlightingOnLoad() deprecated.  Use highlightAll() now.")
},registerLanguage:(e,t)=>{let s=null;try{s=t(n)}catch(t){
if(z("Language definition for '{}' could not be registered.".replace("{}",e)),
!o)throw t;z(t),s=l}
s.name||(s.name=e),i[e]=s,s.rawDefinition=t.bind(null,n),s.aliases&&v(s.aliases,{
languageName:e})},unregisterLanguage:e=>{delete i[e]
;for(const t of Object.keys(s))s[t]===e&&delete s[t]},
listLanguages:()=>Object.keys(i),getLanguage:O,registerAliases:v,
autoDetection:k,inherit:Y,addPlugin:e=>{(e=>{
e["before:highlightBlock"]&&!e["before:highlightElement"]&&(e["before:highlightElement"]=t=>{
e["before:highlightBlock"](Object.assign({block:t.el},t))
}),e["after:highlightBlock"]&&!e["after:highlightElement"]&&(e["after:highlightElement"]=t=>{
e["after:highlightBlock"](Object.assign({block:t.el},t))})})(e),r.push(e)},
removePlugin:e=>{const t=r.indexOf(e);-1!==t&&r.splice(t,1)}}),n.debugMode=()=>{
o=!1},n.safeMode=()=>{o=!0},n.versionString="11.11.1",n.regex={concat:h,
lookahead:g,either:f,optional:d,anyNumberOfTimes:u}
;for(const t in j)"object"==typeof j[t]&&e(j[t]);return Object.assign(n,j),n
},te=ee({});return te.newInstance=()=>ee({}),te}()
;"object"==typeof exports&&"undefined"!=typeof module&&(module.exports=hljs);/*! `ocaml` grammar compiled for Highlight.js 11.11.1 */
(()=>{var e=(()=>{"use strict";return e=>({name:"OCaml",aliases:["ml"],
keywords:{$pattern:"[a-z_]\\w*!?",
keyword:"and as assert asr begin class constraint do done downto else end exception external for fun function functor if in include inherit! inherit initializer land lazy let lor lsl lsr lxor match method!|10 method mod module mutable new object of open! open or private rec sig struct then to try type val! val virtual when while with parser value",
built_in:"array bool bytes char exn|5 float int int32 int64 list lazy_t|5 nativeint|5 string unit in_channel out_channel ref",
literal:"true false"},illegal:/\/\/|>>/,contains:[{className:"literal",
begin:"\\[(\\|\\|)?\\]|\\(\\)",relevance:0},e.COMMENT("\\(\\*","\\*\\)",{
contains:["self"]}),{className:"symbol",begin:"'[A-Za-z_](?!')[\\w']*"},{
className:"type",begin:"`[A-Z][\\w']*"},{className:"type",
begin:"\\b[A-Z][\\w']*",relevance:0},{begin:"[a-z_]\\w*'[\\w']*",relevance:0
},e.inherit(e.APOS_STRING_MODE,{className:"string",relevance:0
}),e.inherit(e.QUOTE_STRING_MODE,{illegal:null}),{className:"number",
begin:"\\b(0[xX][a-fA-F0-9_]+[Lln]?|0[oO][0-7_]+[Lln]?|0[bB][01_]+[Lln]?|[0-9][0-9_]*([Lln]|(\\.[0-9_]*)?([eE][-+]?[0-9_]+)?)?)",
relevance:0},{begin:/->/}]})})();hljs.registerLanguage("ocaml",e)})();
  |}
end

(** Stack frame extracted from backtrace *)
type stack_frame = {
  file: string option;
  line: int option;
  char_range: (int * int) option;
  function_name: string option;
  raw: string;
}

(** Source code snippet with context *)
type source_snippet = {
  start_line: int;
  lines: (int * string) list;  (* line_num, content *)
  error_line: int;
  source_path: string;  (* Clean workspace-relative path *)
  found_via_tusk: bool;  (* Whether tusk server helped resolve *)
}

(** Resolved source path information *)
type resolved_path = {
  resolved_path: string;
  found_via_tusk: bool;
}

(** Sandbox path parsing result *)
type sandbox_info = {
  package_name: string;
  relative_path: string;
}

(** Find substring in string, returns start index *)
let string_index = fun line pattern ->
    let pattern_len = String.length pattern in
    let line_len = String.length line in
    let rec search pos =
      if pos + pattern_len > line_len then
        None
      else if String.sub line pos pattern_len = pattern then
        Some pos
      else
        search (pos + 1)
    in
    search 0

(** Parse sandbox path to extract package and relative path
    
    Input: /path/_build/debug/sandbox/suri-abc123/examples/file.ml
    Output: Some { package_name = "suri"; relative_path = "examples/file.ml" }
*)
let parse_sandbox_path = fun path ->
    match string_index path "/sandbox/" with
    | None -> None
    | Some idx ->
        let after_sandbox = idx + String.length "/sandbox/" in
        if after_sandbox >= String.length path then
          None
        else
          let rest = String.sub path after_sandbox (String.length path - after_sandbox) in
          (* Find first slash to separate package-hash from path *)
          (
            match String.index_opt rest '/' with
            | None -> None
            | Some slash_pos ->
                let pkg_with_hash = String.sub rest 0 slash_pos in
                (* Remove hash suffix: "suri-abc123" -> "suri" *)
                let package_name =
                  match String.rindex_opt pkg_with_hash '-' with
                  | None -> pkg_with_hash
                  | Some dash_pos -> String.sub pkg_with_hash 0 dash_pos
                in
                let after_slash = slash_pos + 1 in
                let relative_path = String.sub rest after_slash (String.length rest - after_slash) in
                Some {package_name; relative_path}
          )

(** Try to connect to tusk server and get package sources *)
let get_package_sources = fun package_name ->
    let cwd = Std.Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
    match Tusk_model.Workspace_manager.scan cwd with
    | Error _ -> None
    | Ok (workspace, _load_errors) -> (
        match List.find_opt (fun (pkg: Tusk_model.Package.t) -> pkg.name = package_name) workspace.packages with
        | None -> None
        | Some pkg ->
            let sources = pkg.sources.src
            @ pkg.sources.tests
            @ pkg.sources.examples
            @ pkg.sources.bench
            @ pkg.sources.native
            |> List.map Path.to_string in
            Some sources
      )

(** Find actual source file path from sandbox path using tusk server *)
let find_source_via_tusk = fun sandbox_info ->
    match get_package_sources sandbox_info.package_name with
    | None -> None
    | Some sources ->
        (* Find source file that ends with the relative path *)
        List.find_opt
          (fun src_file -> String.ends_with ~suffix:sandbox_info.relative_path src_file)
          sources

(** Resolve sandbox path to actual workspace source file
    
    Strategy:
    1. Parse sandbox path to get package + relative path
    2. Query tusk server for package sources
    3. Match against relative path
    4. Return clean workspace-relative path
*)

(** Make a path relative to the workspace root *)
let make_workspace_relative = fun path ->
    let cwd = Std.Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
    match Tusk_model.Workspace_manager.find_workspace_root cwd with
    | None -> path
    | Some workspace_root ->
        let workspace_root_str = Path.to_string workspace_root in
        if String.starts_with ~prefix:workspace_root_str path then
          let prefix_len = String.length workspace_root_str in
          let relative = String.sub path prefix_len (String.length path - prefix_len) in
          (* Remove leading slash if present *)
          if String.length relative > 0 && String.get relative 0 = '/' then
            "." ^ relative
          else
            "./" ^ relative
        else
          path

let resolve_source_path = fun path ->
    match parse_sandbox_path path with
    | None ->
        (* Not a sandbox path, make it relative to workspace *)
        {resolved_path = make_workspace_relative path; found_via_tusk = false}
    | Some sandbox_info -> (* Try to find via tusk server *)
      (
        match find_source_via_tusk sandbox_info with
        | Some actual_path -> {
          resolved_path = make_workspace_relative actual_path;
          found_via_tusk = true
        }
        | None ->
            (* Fallback: construct expected path *)
            let fallback = String.concat
              ""
              [ "./packages/"; sandbox_info.package_name; "/"; sandbox_info.relative_path ] in
            {resolved_path = fallback; found_via_tusk = false}
      )

(** Extract quoted string after a pattern *)
let extract_quoted = fun line pattern ->
    match String.index_opt line '"' with
    | None -> None
    | Some start_quote ->
        let after_quote = start_quote + 1 in
        (
          match String.index_from_opt line after_quote '"' with
          | None -> None
          | Some end_quote -> Some (String.sub line after_quote (end_quote - after_quote))
        )

(** Extract number after a pattern *)
let extract_number = fun line pattern ->
    match string_index line pattern with
    | None -> None
    | Some idx ->
        let after = idx + String.length pattern in
        let rec find_digits acc pos =
          if pos >= String.length line then
            acc
          else
            match line.[pos] with
            | '0' .. '9' as c -> find_digits (acc ^ String.make 1 c) (pos + 1)
            | _ -> acc
        in
        let num_str = find_digits "" after in
        if num_str = "" then
          None
        else
          try Some (Int.of_string num_str) with
          | Failure _ -> None

(** Parse a backtrace line into a stack frame
    
    OCaml backtrace format examples:
    - "Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33"
    - "Called from Mymodule.handler in file "handler.ml", line 42, characters 5-20"
    - "Re-raised at file "main.ml", line 100, characters 10-25"
*)
let parse_frame_line = fun line ->
    let file =
      if String.contains line "\"" then
        extract_quoted line "file"
      else
        None
    in
    let line_num = extract_number line "line " in
    let char_range =
      match extract_number line "characters " with
      | None -> None
      | Some start -> (* Find the dash and extract end number *)
        (
          match string_index line "characters " with
          | None -> None
          | Some idx ->
              let after = idx + String.length "characters " in
              let rest = String.sub line after (String.length line - after) in
              (
                match String.index_opt rest '-' with
                | None -> None
                | Some dash_pos ->
                    let after_dash = dash_pos + 1 in
                    let rec find_digits acc pos =
                      if pos >= String.length rest then
                        acc
                      else
                        match rest.[pos] with
                        | '0' .. '9' as c -> find_digits (acc ^ String.make 1 c) (pos + 1)
                        | _ -> acc
                    in
                    let end_str = find_digits "" after_dash in
                    if end_str = "" then
                      None
                    else
                      try Some (start, Int.of_string end_str) with
                      | Failure _ -> None
              )
        )
    in
    let function_name =
      if String.starts_with ~prefix:"Raised at " line then
        let after = 10 in
        (* length of "Raised at " *)
        let rest = String.sub line after (String.length line - after) in
        (
          match String.index_opt rest ' ' with
          | Some space_pos -> Some (String.sub rest 0 space_pos |> String.trim)
          | None -> Some (String.trim rest)
        )
      else if String.starts_with ~prefix:"Called from " line then
        let after = 12 in
        (* length of "Called from " *)
        let rest = String.sub line after (String.length line - after) in
        (
          match String.index_opt rest ' ' with
          | Some space_pos -> Some (String.sub rest 0 space_pos |> String.trim)
          | None -> Some (String.trim rest)
        )
      else if String.starts_with ~prefix:"Re-raised at " line then
        Some "Re-raised"
      else
        None
    in
    {file; line = line_num; char_range; function_name; raw = line}

(** Check if a frame should be hidden from the stack trace *)
let should_hide_frame = fun frame ->
    match frame.function_name with
    | Some fn ->
        (* Hide panic, raise, and other runtime error handling functions *)
        String.starts_with ~prefix:"Std.Global.panic" fn
        || String.starts_with ~prefix:"Std__Global.panic" fn
        || String.starts_with ~prefix:"Kernel.Global0.panic" fn
        || String.starts_with ~prefix:"Kernel.Global0.raise" fn
        || String.starts_with ~prefix:"Kernel__Global0.panic" fn
        || String.starts_with ~prefix:"Kernel__Global0.raise" fn
        || String.starts_with ~prefix:"Stdlib.raise" fn
        || String.starts_with ~prefix:"Stdlib.failwith" fn
        || String.starts_with ~prefix:"Stdlib.invalid_arg" fn
        || String.starts_with ~prefix:"CamlinternalLazy" fn
    | None -> false

(** Parse full backtrace into list of stack frames *)
let parse_backtrace = fun backtrace ->
    String.split_on_char '\n' backtrace
    |> List.filter (fun line -> String.trim line != "")
    |> List.map parse_frame_line
    |> List.filter (fun frame -> not (should_hide_frame frame))

(** Try to find and read a source file using tusk server resolution *)
let try_read_file = fun file ->
    (* First resolve the path using tusk server if it's a sandbox path *)
    let resolved = resolve_source_path file in
    (* Try to read from the resolved path *)
    match Fs.read_to_string (Path.v resolved.resolved_path) with
    | Ok content -> Some (content, resolved)
    | Error _ -> None

(** Read source file and extract lines around error location *)
let extract_source = fun ~file ~line ~context ->
    match try_read_file file with
    | None -> None
    | Some (content, resolved) ->
        let all_lines = String.split_on_char '\n' content in
        let total_lines = List.length all_lines in
        if line < 1 || line > total_lines then
          None
        else
          let start_line = max 1 (line - context) in
          let end_line = min total_lines (line + context) in
          (* Extract the relevant lines with line numbers *)
          let lines =
            List.init (end_line - start_line + 1)
              (fun i ->
                let line_num = start_line + i in
                let line_content = List.nth all_lines (line_num - 1) in
                (line_num, line_content))
          in
          Some {
            start_line;
            lines;
            error_line = line;
            source_path = resolved.resolved_path;
            found_via_tusk = resolved.found_via_tusk;

          }

(** Render source code snippet *)
let render_snippet = fun snippet ->
    let open Component in
      let line_number_divs =
        List.map
          (fun ((line_num, _)) ->
            let is_error = line_num = snippet.error_line in
            let classes =
              if is_error then
                "line-number highlighted-line"
              else
                "line-number"
            in
            div
              ~attrs:[ class_ classes; attr "data-line-number" (Int.to_string line_num) ]
              [ text (Int.to_string line_num) ])
          snippet.lines
      in
      (* Each line gets its own code block for syntax highlighting *)
      let code_line_divs =
        List.map
          (fun ((line_num, content)) ->
            let is_error = line_num = snippet.error_line in
            let classes =
              if is_error then
                "code-line highlighted-line"
              else
                "code-line"
            in
            div
              ~attrs:[
                class_ classes;
                attr "id" (String.concat "" [ "LC"; Int.to_string line_num ])
              ]
              [ pre [ code ~attrs:[ class_ "language-ocaml" ] [ text content ] ] ])
          snippet.lines
      in
      div
        ~attrs:[ class_ "source-snippet" ]
        [
          div ~attrs:[ class_ "line-numbers" ] line_number_divs;
          div ~attrs:[ class_ "source-code" ] code_line_divs;

        ]

(** Extract module name from function name
    
    Examples:
    - "Debugger_test.process_user_request" -> "Debugger_test"
    - "Suri__Middleware__Debugger.debugger" -> "Suri.Middleware.Debugger"
    - "Std__Global.panic" -> "Std.Global"
    - "Kernel__Global0.raise" -> "Kernel.Global0"
    
    Returns the qualified module name (with dots) for CodeDB lookup.
*)
let extract_module_from_function = fun func_name ->
    (* Split by dot to get module part *)
    match String.index_opt func_name '.' with
    | None -> None
    | Some dot_pos ->
        let module_part = String.sub func_name 0 dot_pos in
        (* Check if it's in canonical form (Foo__Bar__Baz) or simple form (Foo_bar) *)
        if String.contains module_part "_" then
          let components = String.split_on_char '_' module_part in
          let non_empty =
            List.filter (fun s -> s != "") components
          in
          (* If all components start with uppercase, it's canonical form *)
          let all_capitalized =
            List.for_all (fun s -> String.length s > 0 && s.[0] >= 'A' && s.[0] <= 'Z') non_empty
          in
          if all_capitalized then
            Some (String.concat "." non_empty)
          else
            (* Simple or mixed: just capitalize the whole thing *)
            Some (String.capitalize_ascii module_part)
        else
          (* No underscores - just return as-is (already capitalized) *)
          Some module_part

(** Find source file for a module using tusk server *)
let find_source_for_module = fun package_name module_name ->
    match get_package_sources package_name with
    | None -> None
    | Some sources ->
        let ml_name = module_name ^ ".ml" in
        List.find_opt (fun src_file -> String.ends_with ~suffix:ml_name src_file) sources

(** Render a single stack frame with optional source snippet *)
let render_stack_frame = fun frame ->
    let open Component in
      let snippet =
        match (frame.file, frame.line, frame.function_name) with
        | Some file, Some line, _ -> (* Try the file path first *)
          (
            match extract_source ~file ~line ~context:5 with
            | Some s -> Some s
            | None -> (* File path didn't work, try using function name *)
              (
                match frame.function_name with
                | None -> None
                | Some func_name -> (* Extract module name from function *)
                  (
                    match extract_module_from_function func_name with
                    | None -> None
                    | Some module_name ->
                        (* Try to guess package from file path *)
                        let package_guess =
                          match parse_sandbox_path file with
                          | Some si -> si.package_name
                          | None -> "suri"
                        in
                        (* Find source file for this module *)
                        (
                          match find_source_for_module package_guess module_name with
                          | None -> None
                          | Some source_file -> extract_source ~file:source_file ~line ~context:5
                        )
                  )
              )
          )
        | _, Some line, Some func_name -> (* No file path, but we have function name and line *)
          (
            match extract_module_from_function func_name with
            | None -> None
            | Some module_name ->
                (* Try different packages *)
                let packages = [ "suri"; "std"; "kernel"; "http"; "blink" ] in
                let rec try_packages = function
                  | [] -> None
                  | pkg :: rest -> (
                      match find_source_for_module pkg module_name with
                      | None -> try_packages rest
                      | Some source_file -> extract_source ~file:source_file ~line ~context:5
                    )
                in
                try_packages packages
          )
        | _ ->
            None
      in
      (* Determine what file info to display *)
      let file_info, tusk_badge =
        match snippet with
        | Some s ->
            let path_display = s.source_path in
            let info =
              match frame.line with
              | Some l -> path_display ^ ":" ^ Int.to_string l
              | None -> path_display
            in
            let badge =
              if s.found_via_tusk then
                span
                  ~attrs:[ class_ "tusk-badge"; attr "title" "Path resolved via tusk server" ]
                  [ text "✓" ]
              else
                text ""
            in
            (info, badge)
        | None ->
            (* No snippet available - show function name as fallback *)
            let display =
              match frame.function_name with
              | Some fn -> fn
              | None -> (
                  match frame.file with
                  | Some f -> f
                  | None -> "(unknown)"
                )
            in
            let info =
              match frame.line with
              | Some l -> display ^ ":" ^ Int.to_string l
              | None -> display
            in
            (info, text "")
      in
      div ~attrs:[ class_ "stack-frame" ]
        [ div ~attrs:[ class_ "frame-header" ]
            [ span ~attrs:[ class_ "frame-location" ] [ text file_info; text " "; tusk_badge;  ]; (
                match frame.function_name with
                | Some name -> span ~attrs:[ class_ "frame-function" ] [ text (" in " ^ name) ]
                | None -> text ""
              );  ]; (
            match snippet with
            | Some s -> render_snippet s
            | None ->
                (* No source available *)
                div ~attrs:[ class_ "source-unavailable" ] [ text "Source file not available.";  ]
          );  ]

(** Render request inspector *)
let render_request = fun conn ->
    let open Component in
      let method_str = Conn.method_ conn |> Net.Http.Method.to_string in
      let path = Conn.path conn in
      let headers = Conn.headers conn in
      let params = Conn.params conn in
      let body_str = Conn.body conn in
      div ~attrs:[ class_ "request-inspector" ]
        [
          h2 [ text "📨 Request" ];
          div
            ~attrs:[ class_ "request-line" ]
            [ strong [ text (method_str ^ " ") ]; code [ text path ];  ];
          (
            if Net.Http.Header.is_empty headers then
              text ""
            else
              Fragment [ h3 [ text "Headers" ]; table ~attrs:[ class_ "headers-table" ]
                  (Net.Http.Header.to_list headers
                  |> List.map
                    (fun ((name, value)) ->
                      tr
                        [
                          td ~attrs:[ class_ "header-name" ] [ code [ text name ] ];
                          td ~attrs:[ class_ "header-value" ] [ text value ];

                        ]));  ]
          );
          (
            if params = [] then
              text ""
            else
              Fragment [
                h3 [ text "Parameters" ];
                table
                  ~attrs:[ class_ "params-table" ]
                  (List.map
                    (fun ((name, value)) ->
                      tr
                        [
                          td ~attrs:[ class_ "param-name" ] [ code [ text name ] ];
                          td ~attrs:[ class_ "param-value" ] [ text value ];

                        ])
                    params);

              ]
          );
          (
            if body_str = "" then
              text ""
            else
              Fragment [
                h3 [ text "Body" ];
                pre ~attrs:[ class_ "request-body" ] [ text body_str ];

              ]
          );

        ]

(** Render response inspector (shows partial response state) *)
let render_response = fun conn ->
    let open Component in
      let resp_headers = Conn.resp_headers conn in
      let response = Conn.to_response conn in
      let status = response.Web_server.Response.status in
      let status_code = Net.Http.Status.to_int status in
      let status_text = Net.Http.Status.to_string status in
      div ~attrs:[ class_ "response-inspector" ]
        [
          h2 [ text "📤 Response (before error)" ];
          div
            ~attrs:[ class_ "response-status" ]
            [
              strong [ text "Status: " ];
              code [ text (Int.to_string status_code ^ " " ^ status_text) ];

            ];
          (
            if resp_headers = [] then
              text ""
            else
              Fragment [
                h3 [ text "Headers" ];
                table
                  ~attrs:[ class_ "headers-table" ]
                  (List.map
                    (fun ((name, value)) ->
                      tr
                        [
                          td ~attrs:[ class_ "header-name" ] [ code [ text name ] ];
                          td ~attrs:[ class_ "header-value" ] [ text value ];

                        ])
                    resp_headers);

              ]
          );

        ]

(** CSS styles for the error page *)
let error_page_styles = {|
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', sans-serif;
  background: #f5f5f5;
  color: #333;
  line-height: 1.6;
  height: 100vh;
  overflow: hidden;
}

.error-container {
  display: flex;
  flex-direction: column;
  height: 100vh;
}

.error-header {
  background: #fff;
  border-bottom: 1px solid #e0e0e0;
  padding: 20px 30px;
  flex-shrink: 0;
}

.error-header h1 {
  font-size: 18px;
  font-weight: 600;
  color: #e53935;
  margin-bottom: 8px;
  display: flex;
  align-items: center;
  justify-content: space-between;
}

.suri-brand {
  color: #1976d2;
  font-size: 24px;
  font-weight: 700;
  letter-spacing: -0.5px;
}

.error-header .exception-type {
  font-size: 14px;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  color: #666;
  margin-top: 4px;
  word-break: break-all;
}

.error-body {
  display: flex;
  flex: 1;
  overflow: hidden;
}

.left-column {
  flex: 3;
  background: #fff;
  overflow-y: auto;
  border-right: 1px solid #e0e0e0;
}

.right-column {
  flex: 2;
  background: #fafafa;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
}

.section {
  padding: 20px 30px;
}

.section h2 {
  color: #333;
  margin-bottom: 16px;
  font-size: 13px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  display: flex;
  align-items: center;
  gap: 8px;
}

.stack-frame {
  background: #f9f9f9;
  border-left: 3px solid #e53935;
  padding: 12px 16px;
  margin-bottom: 1px;
  font-size: 13px;
  cursor: pointer;
  transition: background 0.15s;
}

.stack-frame:hover {
  background: #f0f0f0;
}

.frame-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 8px;
}

.frame-location {
  color: #1976d2;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 12px;
  font-weight: 500;
}

.frame-function {
  color: #757575;
  font-size: 12px;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
}

.source-snippet {
  background: #fff;
  border: 1px solid #e0e0e0;
  border-radius: 3px;
  overflow-x: auto;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 12px;
  margin-top: 8px;
  display: flex;
}

.line-numbers {
  background: #f9f9f9;
  border-right: 1px solid #e0e0e0;
  padding: 8px 0;
}

.line-number {
  padding: 2px 12px 2px 8px;
  text-align: right;
  color: #9e9e9e;
  font-size: 12px;
  user-select: none;
  line-height: 20px;
  height: 24px;
  min-width: 50px;
  box-sizing: border-box;
}

.line-number.highlighted-line {
  background: #fff3cd;
  border-left: 3px solid #e53935;
}

.source-code {
  flex: 1;
  overflow-x: auto;
  padding: 8px 0;
}

.code-line {
  line-height: 20px;
  height: 24px;
  padding: 2px 12px;
  box-sizing: border-box;
}

.code-line.highlighted-line {
  background: #fff3cd;
}

.code-line pre {
  margin: 0;
  padding: 0;
  display: inline-block;
  width: 100%;
  line-height: 20px;
}

.code-line code {
  background: transparent !important;
  padding: 0 !important;
  margin: 0;
  font-family: inherit;
  font-size: 12px;
  line-height: 20px;
  white-space: pre;
}

.source-line {
  display: flex;
  padding: 2px 12px;
  align-items: baseline;
}

.source-line.error {
  background: #ffebee;
  border-left: 3px solid #e53935;
}

.line-num {
  color: #9e9e9e;
  min-width: 45px;
  text-align: right;
  padding-right: 16px;
  user-select: none;
  font-size: 11px;
}

.line-marker {
  color: #e53935;
  font-weight: bold;
  margin-right: 8px;
  user-select: none;
  width: 12px;
}

.line-content {
  flex: 1;
  padding-left: 12px;
  margin: 0;
}

.line-content code {
  white-space: pre;
  background: transparent !important;
  padding: 0 !important;
  margin: 0;
  font-family: inherit;
  font-size: inherit;
}

.frame-raw {
  background: #fff;
  border: 1px solid #e0e0e0;
  padding: 8px 12px;
  border-radius: 3px;
  color: #666;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 11px;
  overflow-x: auto;
  margin-top: 8px;
}

.request-inspector,
.response-inspector {
  padding: 0;
  border-bottom: 1px solid #e0e0e0;
}

.request-inspector {
  flex: 0 0 auto;
}

.response-inspector {
  flex: 1 0 auto;
}

.request-inspector h2,
.response-inspector h2 {
  color: #333;
  font-size: 13px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 12px;
  padding: 20px 30px 0;
}

.request-inspector h3,
.response-inspector h3 {
  color: #666;
  font-size: 11px;
  margin-top: 16px;
  margin-bottom: 8px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  font-weight: 600;
  padding: 0 30px;
}

.request-line {
  margin-bottom: 16px;
  font-size: 13px;
  padding: 0 30px;
}

.request-line strong {
  color: #e53935;
  font-weight: 600;
}

.request-line code {
  color: #1976d2;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  background: none;
  padding: 0;
}

.response-status {
  margin-bottom: 16px;
  font-size: 13px;
  padding: 0 30px;
}

.response-status code {
  color: #388e3c;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  background: none;
  padding: 0;
}

table {
  width: 100%;
  border-collapse: collapse;
  font-size: 12px;
}

tr {
  border-bottom: 1px solid #f0f0f0;
}

td {
  padding: 8px 30px;
  vertical-align: top;
}

.header-name,
.param-name {
  width: 35%;
  color: #757575;
  font-weight: 500;
}

.header-name code,
.param-name code {
  color: #757575;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 11px;
  background: none;
  padding: 0;
}

.header-value,
.param-value {
  color: #424242;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 11px;
  word-break: break-all;
}

.request-body {
  background: #fff;
  border: 1px solid #e0e0e0;
  padding: 12px;
  border-radius: 3px;
  color: #333;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 11px;
  overflow-x: auto;
  max-height: 300px;
  overflow-y: auto;
  margin: 0 30px 20px;
}

.tusk-badge {
  display: inline-block;
  color: #4caf50;
  font-size: 14px;
  font-weight: bold;
  margin-left: 6px;
  vertical-align: middle;
}

.source-unavailable {
  background: #fff3cd;
  border: 1px solid #ffc107;
  border-left: 3px solid #ffc107;
  padding: 12px 16px;
  border-radius: 3px;
  color: #856404;
  font-size: 12px;
  margin-top: 8px;
  font-style: italic;
}

code {
  background: none;
  padding: 0;
  border-radius: 0;
}

strong {
  font-weight: 600;
}

details {
  margin-top: 20px;
  padding: 16px;
  background: #f5f5f5;
  border: 1px solid #e0e0e0;
  border-radius: 4px;
}

summary {
  cursor: pointer;
  font-weight: 600;
  color: #666;
  font-size: 13px;
  user-select: none;
  margin-bottom: 12px;
}

summary:hover {
  color: #333;
}

details[open] summary {
  margin-bottom: 12px;
}

.raw-backtrace {
  background: #fff;
  border: 1px solid #e0e0e0;
  padding: 12px;
  border-radius: 3px;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 11px;
  color: #333;
  white-space: pre-wrap;
  word-break: break-all;
  overflow-x: auto;
}
|}

(** Main error page component *)
let render_error_page = fun ~conn ~exn ~backtrace ->
    let open Component in
      let exception_str = Exception.to_string exn in
      let frames = parse_backtrace backtrace in
      let method_str = Conn.method_ conn |> Net.Http.Method.to_string in
      let path = Conn.path conn in
      html
        [
          head
            [
              meta ~attrs:[ attr "charset" "UTF-8" ] ();
              meta ~attrs:[ attr "viewport" "width=device-width, initial-scale=1.0" ] ();
              title [ text "500 Internal Server Error" ];
              link
                ~attrs:[
                  attr "rel" "stylesheet";
                  attr "href" "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/default.min.css"
                ]
                ();
              style error_page_styles;
              script
                ~attrs:[
                  attr "src" "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js"
                ]
                "";
              script
                ~attrs:[
                  attr "src" "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/ocaml.min.js"
                ]
                "";
              script "hljs.highlightAll();";

            ];
          body
            [
              div
                ~attrs:[ class_ "error-container" ]
                [
                  div
                    ~attrs:[ class_ "error-header" ]
                    [
                      h1
                        [
                          span [ text (exception_str ^ " at " ^ method_str ^ " " ^ path) ];
                          span ~attrs:[ class_ "suri-brand" ] [ text "SURI" ];

                        ];
                      div ~attrs:[ class_ "exception-type" ] [ text exception_str ];

                    ];
                  div
                    ~attrs:[ class_ "error-body" ]
                    [
                      div
                        ~attrs:[ class_ "left-column" ]
                        [
                          div
                            ~attrs:[ class_ "section" ]
                            [
                              h2 [ text "📚 Stack Trace" ];
                              Fragment (List.map render_stack_frame frames);
                              details
                                [
                                  summary [ text "🔍 Show Raw Backtrace" ];
                                  div ~attrs:[ class_ "raw-backtrace" ] [ text backtrace ];

                                ];

                            ];

                        ];
                      div
                        ~attrs:[ class_ "right-column" ]
                        [ render_request conn; render_response conn;  ];

                    ];

                ];

            ];

        ]

(** Debugger middleware - catches exceptions, displays error page, logs, and reraises *)
let debugger = fun ~conn ~next ->
    try next conn with
    | exn ->
        (* Capture backtrace immediately *)
        let backtrace = Exception.get_backtrace () in
        let exception_str = Exception.to_string exn in
        let method_str = Conn.method_ conn |> Net.Http.Method.to_string in
        let path = Conn.path conn in
        (* Log the error *)
        Log.error (String.concat "" [ method_str; " "; path; " -> Exception: "; exception_str ]);
        (* Build error page *)
        let error_page = render_error_page ~conn ~exn ~backtrace in
        (* Set 500 error response *)
        let _ = conn
        |> Conn.with_status InternalServerError
        |> Conn.with_header "Content-Type" "text/html; charset=utf-8"
        |> Conn.with_body (Component.to_html error_page)
        |> Conn.send in
        (* Reraise the exception after writing response *)
        raise exn
