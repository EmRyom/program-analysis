function fallbackCopyTextToClipboard(text) {
  var textArea = document.createElement("textarea");
  textArea.value = text;
  
  // Avoid scrolling to bottom
  textArea.style.top = "0";
  textArea.style.left = "0";
  textArea.style.position = "fixed";

  document.body.appendChild(textArea);
  textArea.focus();
  textArea.select();

  try {
    var successful = document.execCommand('copy');
  } catch (err) {
    console.error('Unable to copy', err);
  }

  document.body.removeChild(textArea);
}
function copyTextToClipboard(text) {
  if (!navigator.clipboard) {
    fallbackCopyTextToClipboard(text);
    return;
  }
  navigator.clipboard.writeText(text.replaceAll("-&gt;","->"));
}

var copyBtn = document.querySelector('#clipboard-btn');
var outputElem = document.querySelector('#output');

if (copyBtn) {
    copyBtn.addEventListener('click', function(event) {
	copyTextToClipboard(outputElem.innerHTML.slice(0, -1));
    });
}

var examples =
    "(*\n  Author: Jørgen Villadsen, DTU Compute, 2021\n*)\n\n# \"Example 1\"\n\nDis p[a, b] (Neg p[a, b])\n\nAlphaDis\n  p[a, b]\n  Neg p[a, b]\nBasic\n\n# \"Example 2\"\n\nImp (Uni (Uni (p[1, 0]))) p[a, a]\n\nAlphaImp\n  Neg (Uni (Uni p[1, 0]))\n  p[a, a]\nGammaUni\n  Neg (Uni p[a, 0])\n  p[a, a]\nGammaUni\n  Neg p[a, a]\n  p[a, a]\nExt\n  p[a, a]\n  Neg p[a, a]\nBasic\n\n# \"Example 3\"\n\nImp (Uni (Imp p[0] q[0])) (Imp (Exi p[0]) (Exi q[0]))\n\nAlphaImp\n  Neg (Uni (Imp p[0] q[0]))\n  Imp (Exi p[0]) (Exi q[0])\nExt\n  Imp (Exi p[0]) (Exi q[0])\n  Neg (Uni (Imp p[0] q[0]))\nAlphaImp\n  Neg (Exi p[0])\n  Exi q[0]\n  Neg (Uni (Imp p[0] q[0]))\nDeltaExi\n  Neg p[a]\n  Exi q[0]\n  Neg (Uni (Imp p[0] q[0]))\nExt\n  Neg (Uni (Imp p[0] q[0]))\n  Neg p[a]\n  Exi q[0]\nGammaUni\n  Neg (Imp p[a] q[a])\n  Neg p[a]\n  Exi q[0]\nBetaImp\n  p[a]\n  Neg p[a]\n  Exi q[0]\n+\n  Neg q[a]\n  Neg p[a]\n  Exi q[0]\nBasic\n  Neg q[a]\n  Neg p[a]\n  Exi q[0]\nExt\n  Exi q[0]\n  Neg q[a]\nGammaExi\n  q[a]\n  Neg q[a]\nBasic\n\n(*\n  Source: https://secav.compute.dtu.dk/\n*)\n"

var exBtn = document.querySelector('#example-btn');
if (exBtn) {
    exBtn.addEventListener('click', function(event) {
	copyTextToClipboard(examples);
    });
}

var inputArea = document.querySelector('#input');
var positionText = document.querySelector('#position');

if (inputArea) {
    inputArea.addEventListener('keyup', function(event) {
	getLineNumber(inputArea, positionText);
    });

    inputArea.addEventListener('mouseup', function(event) {
	getLineNumber(inputArea, positionText);
    });
}

function getLineNumber(input, display) {
    var lines =  input.value.substr(0, input.selectionStart).split("\n");
    var lineNumber = lines.length;
    var columnNumber = lines[lines.length - 1].length +1;
    display.innerHTML = lineNumber + ":" + columnNumber;
}
