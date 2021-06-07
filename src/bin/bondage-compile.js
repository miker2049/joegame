#!/usr/bin/env node

'use strict';

const fs = require('fs');
const program = require('commander');
const Parser = require('../parser/parser.js').parser;
const Lexer = require('../lexer/lexer')
const yy = require('../parser/nodes')
Parser.lexer = new Lexer()
Parser.yy = yy
const readYarn = require('../readYarnFile')


function parseYarnOrJSON(raw){
    try {
     return JSON.parse(raw);
    } catch (e) {
      return readYarn(raw.toString())
    }
}
function showTokens(files) {
  // First, load all of the files that we were given
  files.forEach((file) => {
    console.log(data, "hmmm")
    const data = parseYarnOrJSON(fs.readFileSync(file))
    if (data.length < 1) throw new Error('cant parse file')
    data.forEach((dialog) => {
      const lexer = new Lexer();
      lexer.setInput(dialog.body);
      let token = '';
      console.log('Debug: Tokens:');
      while (token !== 'EndOfInput' && token !== 'Invalid') {
        token = lexer.lex();
        const yytext = lexer.yytext !== '' ? `(${lexer.yytext}) ` : '';
        console.log(`${token} ${yytext}at ${lexer.yylineno}:${lexer.yylloc.first_column} ( line ${lexer.yylineno})`);
      }
      console.log("");
    });
  });
}

function showParse(files) {
  const rootNode = new Parser.yy.RootNode();
  // First, load all of the files that we were given
  files.forEach((file) => {
    const data = parseYarnOrJSON(fs.readFileSync(file))
    data.forEach((dialog) => {
      const rootDialogNode = new Parser.yy.DialogNode(Parser.parse(dialog.body), dialog.title);
      rootNode.dialogNodes.push(rootDialogNode);
    });
  });

  console.log(JSON.stringify(rootNode, null, '  '));
}

program
  .description('Compile given yarn file')
  .option('-t, --tokens', 'Display the tokns emitted by the lexer')
  .option('-a, --ast', 'Display the tree parsed by the parser')
  .arguments('<file...>')
  .action((files, options) => {
    if (options.tokens) {
      showTokens(files);
    } else if (options.parse) {
      showParse(files);
    }
  });

program.parse(process.argv);
