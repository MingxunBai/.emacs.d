#!/usr/bin/env node

const fs = require("fs");

fs.readFile(process.env.HOME + "/.emacs.d/plugins/wb.txt", function (err, data) {
  if (err) console.log(err)
  var lines = data.toString().split("\n");
  var newLines = [
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">\n',
    '<plist version="1.0">\n',
    '<array>\n'
  ];

  for (var i = 44; i < lines.length; i++) {
    var codes = lines[i].split(" ");
    for (var j = 1; j < codes.length; j++) {
      newLines.push('	<dict>\n');
      newLines.push('		<key>phrase</key>\n');
      newLines.push(`		<string>${codes[j]}</string>\n`);
      newLines.push('		<key>shortcut</key>\n');
      newLines.push(`		<string>${codes[0]}</string>\n`);
      newLines.push('	</dict>\n');
    }
  }

  newLines.push('</array>\n');
  newLines.push('</plist>\n');

  fs.writeFile(process.env.HOME + "/Desktop/wb.plist", newLines.join(""), function (err) {
    if (err) console.log(err)
    else console.log("Write success!");
  })
})
