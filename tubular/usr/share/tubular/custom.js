function on_ethertype1_change() {
  ethertype = document.getElementById("ethertype1").value
  if (ethertype == "0x8100") {
    document.getElementById("ethtype2-div").style.display = "block"
    document.getElementById("ethtype3-div").style.display = "none"
  } else if (ethertype == "0x9100" || ethertype == "0x88A8") {
    // it's not clear which of these is the double-tagged ethtype, both are cited inconsistently
    document.getElementById("ethtype2-div").style.display = "block"
    document.getElementById("ethtype3-div").style.display = "block"
  } else {
    document.getElementById("ethtype2-div").style.display = "none"
    document.getElementById("ethtype3-div").style.display = "none"
  }
}

(function(){
})();

