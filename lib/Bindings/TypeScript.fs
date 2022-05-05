// ts2fable 0.0.0
module rec TypeScript

#nowarn "3390" // disable warnings for invalid XML comments
#nowarn "0044" // disable warnings for `Obsolete` usage

open System
open Fable.Core
open Fable.Core.JS

type Array<'T> = System.Collections.Generic.IList<'T>
type ReadonlyArray<'T> = System.Collections.Generic.IReadOnlyList<'T>
type ReadonlySet<'T> = Set<'T>
type Symbol = obj

let [<ImportAll("typescript")>] ts: Ts.IExports = jsNative

type [<AllowNullLiteral>] IExports =
    abstract setTimeout: handler: (obj option[] -> unit) * timeout: float -> obj option
    abstract clearTimeout: handle: obj option -> unit

module Ts =
    let [<Import("ScriptSnapshot","typescript/ts")>] scriptSnapshot: ScriptSnapshot.IExports = jsNative

    type [<AllowNullLiteral>] IExports =
        abstract versionMajorMinor: obj
        /// The version of the TypeScript compiler release
        abstract version: string
        abstract OperationCanceledException: OperationCanceledExceptionStatic
        abstract getNodeMajorVersion: unit -> float option
        abstract sys: System with get, set
        abstract tokenToString: t: SyntaxKind -> string option
        abstract getPositionOfLineAndCharacter: sourceFile: SourceFileLike * line: float * character: float -> float
        abstract getLineAndCharacterOfPosition: sourceFile: SourceFileLike * position: float -> LineAndCharacter
        abstract isWhiteSpaceLike: ch: float -> bool
        /// Does not include line breaks. For that, see isWhiteSpaceLike.
        abstract isWhiteSpaceSingleLine: ch: float -> bool
        abstract isLineBreak: ch: float -> bool
        abstract couldStartTrivia: text: string * pos: float -> bool
        abstract forEachLeadingCommentRange: text: string * pos: float * cb: (float -> float -> CommentKind -> bool -> 'U) -> 'U option
        abstract forEachLeadingCommentRange: text: string * pos: float * cb: (float -> float -> CommentKind -> bool -> 'T -> 'U) * state: 'T -> 'U option
        abstract forEachTrailingCommentRange: text: string * pos: float * cb: (float -> float -> CommentKind -> bool -> 'U) -> 'U option
        abstract forEachTrailingCommentRange: text: string * pos: float * cb: (float -> float -> CommentKind -> bool -> 'T -> 'U) * state: 'T -> 'U option
        abstract reduceEachLeadingCommentRange: text: string * pos: float * cb: (float -> float -> CommentKind -> bool -> 'T -> 'U -> 'U) * state: 'T * initial: 'U -> 'U option
        abstract reduceEachTrailingCommentRange: text: string * pos: float * cb: (float -> float -> CommentKind -> bool -> 'T -> 'U -> 'U) * state: 'T * initial: 'U -> 'U option
        abstract getLeadingCommentRanges: text: string * pos: float -> CommentRange[] option
        abstract getTrailingCommentRanges: text: string * pos: float -> CommentRange[] option
        /// Optionally, get the shebang
        abstract getShebang: text: string -> string option
        abstract isIdentifierStart: ch: float * languageVersion: ScriptTarget option -> bool
        abstract isIdentifierPart: ch: float * languageVersion: ScriptTarget option * ?identifierVariant: LanguageVariant -> bool
        abstract createScanner: languageVersion: ScriptTarget * skipTrivia: bool * ?languageVariant: LanguageVariant * ?textInitial: string * ?onError: ErrorCallback * ?start: float * ?length: float -> Scanner
        abstract isExternalModuleNameRelative: moduleName: string -> bool
        abstract sortAndDeduplicateDiagnostics: diagnostics: 'T[] -> SortedReadonlyArray<'T>
        abstract getDefaultLibFileName: options: CompilerOptions -> string
        abstract textSpanEnd: span: TextSpan -> float
        abstract textSpanIsEmpty: span: TextSpan -> bool
        abstract textSpanContainsPosition: span: TextSpan * position: float -> bool
        abstract textSpanContainsTextSpan: span: TextSpan * other: TextSpan -> bool
        abstract textSpanOverlapsWith: span: TextSpan * other: TextSpan -> bool
        abstract textSpanOverlap: span1: TextSpan * span2: TextSpan -> TextSpan option
        abstract textSpanIntersectsWithTextSpan: span: TextSpan * other: TextSpan -> bool
        abstract textSpanIntersectsWith: span: TextSpan * start: float * length: float -> bool
        abstract decodedTextSpanIntersectsWith: start1: float * length1: float * start2: float * length2: float -> bool
        abstract textSpanIntersectsWithPosition: span: TextSpan * position: float -> bool
        abstract textSpanIntersection: span1: TextSpan * span2: TextSpan -> TextSpan option
        abstract createTextSpan: start: float * length: float -> TextSpan
        abstract createTextSpanFromBounds: start: float * ``end``: float -> TextSpan
        abstract textChangeRangeNewSpan: range: TextChangeRange -> TextSpan
        abstract textChangeRangeIsUnchanged: range: TextChangeRange -> bool
        abstract createTextChangeRange: span: TextSpan * newLength: float -> TextChangeRange
        abstract unchangedTextChangeRange: TextChangeRange with get, set
        /// Called to merge all the changes that occurred across several versions of a script snapshot
        /// into a single change.  i.e. if a user keeps making successive edits to a script we will
        /// have a text change from V1 to V2, V2 to V3, ..., Vn.
        ///
        /// This function will then merge those changes into a single change range valid between V1 and
        /// Vn.
        abstract collapseTextChangeRangesAcrossMultipleVersions: changes: TextChangeRange[] -> TextChangeRange
        abstract getTypeParameterOwner: d: Declaration -> Declaration option
        abstract isParameterPropertyDeclaration: node: Node * parent: Node -> bool
        abstract isEmptyBindingPattern: node: BindingName -> bool
        abstract isEmptyBindingElement: node: BindingElement -> bool
        abstract walkUpBindingElementsAndPatterns: binding: BindingElement -> U2<VariableDeclaration, ParameterDeclaration>
        abstract getCombinedModifierFlags: node: Declaration -> ModifierFlags
        abstract getCombinedNodeFlags: node: Node -> NodeFlags
        /// Checks to see if the locale is in the appropriate format,
        /// and if it is, attempts to set the appropriate language.
        abstract validateLocaleAndSetLanguage: locale: string * sys: {| getExecutingFilePath: unit -> string; resolvePath: string -> string; fileExists: string -> bool; readFile: string -> string option |} * ?errors: Push<Diagnostic> -> unit
        abstract getOriginalNode: node: Node -> Node
        abstract getOriginalNode: node: Node * nodeTest: (Node -> bool) -> 'T
        abstract getOriginalNode: node: Node option -> Node option
        abstract getOriginalNode: node: Node option * nodeTest: (Node option -> bool) -> 'T option
        /// Iterates through the parent chain of a node and performs the callback on each parent until the callback
        /// returns a truthy value, then returns that value.
        /// If no such value is found, it applies the callback until the parent pointer is undefined or the callback returns "quit"
        /// At that point findAncestor returns undefined.
        abstract findAncestor: node: Node option * callback: (Node -> bool) -> 'T option
        abstract findAncestor: node: Node option * callback: (Node -> U2<bool, string>) -> Node option
        /// <summary>Gets a value indicating whether a node originated in the parse tree.</summary>
        /// <param name="node">The node to test.</param>
        abstract isParseTreeNode: node: Node -> bool
        /// <summary>Gets the original parse tree node for a node.</summary>
        /// <param name="node">The original node.</param>
        /// <returns>The original parse tree node if found; otherwise, undefined.</returns>
        abstract getParseTreeNode: node: Node option -> Node option
        /// <summary>Gets the original parse tree node for a node.</summary>
        /// <param name="node">The original node.</param>
        /// <param name="nodeTest">A callback used to ensure the correct type of parse tree node is returned.</param>
        /// <returns>The original parse tree node if found; otherwise, undefined.</returns>
        abstract getParseTreeNode: node: 'T option * ?nodeTest: (Node -> bool) -> 'T option
        /// Add an extra underscore to identifiers that start with two underscores to avoid issues with magic names like '__proto__'
        abstract escapeLeadingUnderscores: identifier: string -> __String
        /// <summary>Remove extra underscore from escaped identifier text content.</summary>
        /// <param name="identifier">The escaped identifier text.</param>
        /// <returns>The unescaped identifier text.</returns>
        abstract unescapeLeadingUnderscores: identifier: __String -> string
        abstract idText: identifierOrPrivateName: U2<Identifier, PrivateIdentifier> -> string
        abstract symbolName: symbol: Symbol -> string
        abstract getNameOfJSDocTypedef: declaration: JSDocTypedefTag -> U2<Identifier, PrivateIdentifier> option
        abstract getNameOfDeclaration: declaration: U2<Declaration, Expression> option -> DeclarationName option
        /// <summary>Gets the JSDoc parameter tags for the node if present.</summary>
        /// <remarks>
        /// Returns any JSDoc param tag whose name matches the provided
        /// parameter, whether a param tag on a containing function
        /// expression, or a param tag on a variable declaration whose
        /// initializer is the containing function. The tags closest to the
        /// node are returned first, so in the previous example, the param
        /// tag on the containing function expression would be first.
        ///
        /// For binding patterns, parameter tags are matched by position.
        /// </remarks>
        abstract getJSDocParameterTags: param: ParameterDeclaration -> JSDocParameterTag[]
        /// <summary>Gets the JSDoc type parameter tags for the node if present.</summary>
        /// <remarks>
        /// Returns any JSDoc template tag whose names match the provided
        /// parameter, whether a template tag on a containing function
        /// expression, or a template tag on a variable declaration whose
        /// initializer is the containing function. The tags closest to the
        /// node are returned first, so in the previous example, the template
        /// tag on the containing function expression would be first.
        /// </remarks>
        abstract getJSDocTypeParameterTags: param: TypeParameterDeclaration -> JSDocTemplateTag[]
        /// <summary>Return true if the node has JSDoc parameter tags.</summary>
        /// <remarks>
        /// Includes parameter tags that are not directly on the node,
        /// for example on a variable declaration whose initializer is a function expression.
        /// </remarks>
        abstract hasJSDocParameterTags: node: U2<FunctionLikeDeclaration, SignatureDeclaration> -> bool
        /// Gets the JSDoc augments tag for the node if present
        abstract getJSDocAugmentsTag: node: Node -> JSDocAugmentsTag option
        /// Gets the JSDoc implements tags for the node if present
        abstract getJSDocImplementsTags: node: Node -> JSDocImplementsTag[]
        /// Gets the JSDoc class tag for the node if present
        abstract getJSDocClassTag: node: Node -> JSDocClassTag option
        /// Gets the JSDoc public tag for the node if present
        abstract getJSDocPublicTag: node: Node -> JSDocPublicTag option
        /// Gets the JSDoc private tag for the node if present
        abstract getJSDocPrivateTag: node: Node -> JSDocPrivateTag option
        /// Gets the JSDoc protected tag for the node if present
        abstract getJSDocProtectedTag: node: Node -> JSDocProtectedTag option
        /// Gets the JSDoc protected tag for the node if present
        abstract getJSDocReadonlyTag: node: Node -> JSDocReadonlyTag option
        abstract getJSDocOverrideTagNoCache: node: Node -> JSDocOverrideTag option
        /// Gets the JSDoc deprecated tag for the node if present
        abstract getJSDocDeprecatedTag: node: Node -> JSDocDeprecatedTag option
        /// Gets the JSDoc enum tag for the node if present
        abstract getJSDocEnumTag: node: Node -> JSDocEnumTag option
        /// Gets the JSDoc this tag for the node if present
        abstract getJSDocThisTag: node: Node -> JSDocThisTag option
        /// Gets the JSDoc return tag for the node if present
        abstract getJSDocReturnTag: node: Node -> JSDocReturnTag option
        /// Gets the JSDoc template tag for the node if present
        abstract getJSDocTemplateTag: node: Node -> JSDocTemplateTag option
        /// Gets the JSDoc type tag for the node if present and valid
        abstract getJSDocTypeTag: node: Node -> JSDocTypeTag option
        /// <summary>Gets the type node for the node if provided via JSDoc.</summary>
        /// <remarks>
        /// The search includes any JSDoc param tag that relates
        /// to the provided parameter, for example a type tag on the
        /// parameter itself, or a param tag on a containing function
        /// expression, or a param tag on a variable declaration whose
        /// initializer is the containing function. The tags closest to the
        /// node are examined first, so in the previous example, the type
        /// tag directly on the node would be returned.
        /// </remarks>
        abstract getJSDocType: node: Node -> TypeNode option
        /// <summary>Gets the return type node for the node if provided via JSDoc return tag or type tag.</summary>
        /// <remarks>
        /// <c>getJSDocReturnTag</c> just gets the whole JSDoc tag. This function
        /// gets the type from inside the braces, after the fat arrow, etc.
        /// </remarks>
        abstract getJSDocReturnType: node: Node -> TypeNode option
        /// Get all JSDoc tags related to a node, including those on parent nodes.
        abstract getJSDocTags: node: Node -> JSDocTag[]
        /// Gets all JSDoc tags that match a specified predicate
        abstract getAllJSDocTags: node: Node * predicate: (JSDocTag -> bool) -> 'T[]
        /// Gets all JSDoc tags of a specified kind
        abstract getAllJSDocTagsOfKind: node: Node * kind: SyntaxKind -> JSDocTag[]
        /// Gets the text of a jsdoc comment, flattening links to their text.
        abstract getTextOfJSDocComment: ?comment: U2<string, JSDocComment[]> -> string option
        /// <summary>
        /// Gets the effective type parameters. If the node was parsed in a
        /// JavaScript file, gets the type parameters from the <c>@template</c> tag from JSDoc.
        /// </summary>
        abstract getEffectiveTypeParameterDeclarations: node: DeclarationWithTypeParameters -> TypeParameterDeclaration[]
        abstract getEffectiveConstraintOfTypeParameter: node: TypeParameterDeclaration -> TypeNode option
        abstract isMemberName: node: Node -> bool
        abstract isPropertyAccessChain: node: Node -> bool
        abstract isElementAccessChain: node: Node -> bool
        abstract isCallChain: node: Node -> bool
        abstract isOptionalChain: node: Node -> bool
        abstract isNullishCoalesce: node: Node -> bool
        abstract isConstTypeReference: node: Node -> bool
        abstract skipPartiallyEmittedExpressions: node: Expression -> Expression
        abstract skipPartiallyEmittedExpressions: node: Node -> Node
        abstract isNonNullChain: node: Node -> bool
        abstract isBreakOrContinueStatement: node: Node -> bool
        abstract isNamedExportBindings: node: Node -> bool
        abstract isUnparsedTextLike: node: Node -> bool
        abstract isUnparsedNode: node: Node -> bool
        abstract isJSDocPropertyLikeTag: node: Node -> bool
        /// True if kind is of some token syntax kind.
        /// For example, this is true for an IfKeyword but not for an IfStatement.
        /// Literals are considered tokens, except TemplateLiteral, but does include TemplateHead/Middle/Tail.
        abstract isTokenKind: kind: SyntaxKind -> bool
        /// True if node is of some token syntax kind.
        /// For example, this is true for an IfKeyword but not for an IfStatement.
        /// Literals are considered tokens, except TemplateLiteral, but does include TemplateHead/Middle/Tail.
        abstract isToken: n: Node -> bool
        abstract isLiteralExpression: node: Node -> bool
        abstract isTemplateLiteralToken: node: Node -> bool
        abstract isTemplateMiddleOrTemplateTail: node: Node -> bool
        abstract isImportOrExportSpecifier: node: Node -> bool
        abstract isTypeOnlyImportOrExportDeclaration: node: Node -> bool
        abstract isAssertionKey: node: Node -> bool
        abstract isStringTextContainingNode: node: Node -> bool
        abstract isModifier: node: Node -> bool
        abstract isEntityName: node: Node -> bool
        abstract isPropertyName: node: Node -> bool
        abstract isBindingName: node: Node -> bool
        abstract isFunctionLike: node: Node option -> bool
        abstract isClassElement: node: Node -> bool
        abstract isClassLike: node: Node -> bool
        abstract isAccessor: node: Node -> bool
        abstract isTypeElement: node: Node -> bool
        abstract isClassOrTypeElement: node: Node -> bool
        abstract isObjectLiteralElementLike: node: Node -> bool
        /// <summary>
        /// Node test that determines whether a node is a valid type node.
        /// This differs from the <c>isPartOfTypeNode</c> function which determines whether a node is *part*
        /// of a TypeNode.
        /// </summary>
        abstract isTypeNode: node: Node -> bool
        abstract isFunctionOrConstructorTypeNode: node: Node -> bool
        abstract isPropertyAccessOrQualifiedName: node: Node -> bool
        abstract isCallLikeExpression: node: Node -> bool
        abstract isCallOrNewExpression: node: Node -> bool
        abstract isTemplateLiteral: node: Node -> bool
        abstract isAssertionExpression: node: Node -> bool
        abstract isIterationStatement: node: Node * lookInLabeledStatements: bool -> bool
        abstract isJsxOpeningLikeElement: node: Node -> bool
        abstract isCaseOrDefaultClause: node: Node -> bool
        /// True if node is of a kind that may contain comment text.
        abstract isJSDocCommentContainingNode: node: Node -> bool
        abstract isSetAccessor: node: Node -> bool
        abstract isGetAccessor: node: Node -> bool
        /// True if has initializer node attached to it.
        abstract hasOnlyExpressionInitializer: node: Node -> bool
        abstract isObjectLiteralElement: node: Node -> bool
        abstract isStringLiteralLike: node: Node -> bool
        abstract isJSDocLinkLike: node: Node -> bool
        abstract factory: NodeFactory
        abstract createUnparsedSourceFile: text: string -> UnparsedSource
        abstract createUnparsedSourceFile: inputFile: InputFiles * ``type``: IExportsCreateUnparsedSourceFile * ?stripInternal: bool -> UnparsedSource
        abstract createUnparsedSourceFile: text: string * mapPath: string option * map: string option -> UnparsedSource
        abstract createInputFiles: javascriptText: string * declarationText: string -> InputFiles
        abstract createInputFiles: readFileText: (string -> string option) * javascriptPath: string * javascriptMapPath: string option * declarationPath: string * declarationMapPath: string option * buildInfoPath: string option -> InputFiles
        abstract createInputFiles: javascriptText: string * declarationText: string * javascriptMapPath: string option * javascriptMapText: string option * declarationMapPath: string option * declarationMapText: string option -> InputFiles
        /// Create an external source map source file reference
        abstract createSourceMapSource: fileName: string * text: string * ?skipTrivia: (float -> float) -> SourceMapSource
        abstract setOriginalNode: node: 'T * original: Node option -> 'T
        /// <summary>Clears any <c>EmitNode</c> entries from parse-tree nodes.</summary>
        /// <param name="sourceFile">A source file.</param>
        abstract disposeEmitNodes: sourceFile: SourceFile option -> unit
        /// Sets flags that control emit behavior of a node.
        abstract setEmitFlags: node: 'T * emitFlags: EmitFlags -> 'T
        /// Gets a custom text range to use when emitting source maps.
        abstract getSourceMapRange: node: Node -> SourceMapRange
        /// Sets a custom text range to use when emitting source maps.
        abstract setSourceMapRange: node: 'T * range: SourceMapRange option -> 'T
        /// Gets the TextRange to use for source maps for a token of a node.
        abstract getTokenSourceMapRange: node: Node * token: SyntaxKind -> SourceMapRange option
        /// Sets the TextRange to use for source maps for a token of a node.
        abstract setTokenSourceMapRange: node: 'T * token: SyntaxKind * range: SourceMapRange option -> 'T
        /// Gets a custom text range to use when emitting comments.
        abstract getCommentRange: node: Node -> TextRange
        /// Sets a custom text range to use when emitting comments.
        abstract setCommentRange: node: 'T * range: TextRange -> 'T
        abstract getSyntheticLeadingComments: node: Node -> SynthesizedComment[] option
        abstract setSyntheticLeadingComments: node: 'T * comments: SynthesizedComment[] option -> 'T
        abstract addSyntheticLeadingComment: node: 'T * kind: SyntaxKind * text: string * ?hasTrailingNewLine: bool -> 'T
        abstract getSyntheticTrailingComments: node: Node -> SynthesizedComment[] option
        abstract setSyntheticTrailingComments: node: 'T * comments: SynthesizedComment[] option -> 'T
        abstract addSyntheticTrailingComment: node: 'T * kind: SyntaxKind * text: string * ?hasTrailingNewLine: bool -> 'T
        abstract moveSyntheticComments: node: 'T * original: Node -> 'T
        /// Gets the constant value to emit for an expression representing an enum.
        abstract getConstantValue: node: AccessExpression -> U2<string, float> option
        /// Sets the constant value to emit for an expression.
        abstract setConstantValue: node: AccessExpression * value: U2<string, float> -> AccessExpression
        /// Adds an EmitHelper to a node.
        abstract addEmitHelper: node: 'T * helper: EmitHelper -> 'T
        /// Add EmitHelpers to a node.
        abstract addEmitHelpers: node: 'T * helpers: EmitHelper[] option -> 'T
        /// Removes an EmitHelper from a node.
        abstract removeEmitHelper: node: Node * helper: EmitHelper -> bool
        /// Gets the EmitHelpers of a node.
        abstract getEmitHelpers: node: Node -> EmitHelper[] option
        /// Moves matching emit helpers from a source node to a target node.
        abstract moveEmitHelpers: source: Node * target: Node * predicate: (EmitHelper -> bool) -> unit
        abstract isNumericLiteral: node: Node -> bool
        abstract isBigIntLiteral: node: Node -> bool
        abstract isStringLiteral: node: Node -> bool
        abstract isJsxText: node: Node -> bool
        abstract isRegularExpressionLiteral: node: Node -> bool
        abstract isNoSubstitutionTemplateLiteral: node: Node -> bool
        abstract isTemplateHead: node: Node -> bool
        abstract isTemplateMiddle: node: Node -> bool
        abstract isTemplateTail: node: Node -> bool
        abstract isDotDotDotToken: node: Node -> bool
        abstract isPlusToken: node: Node -> bool
        abstract isMinusToken: node: Node -> bool
        abstract isAsteriskToken: node: Node -> bool
        abstract isIdentifier: node: Node -> bool
        abstract isPrivateIdentifier: node: Node -> bool
        abstract isQualifiedName: node: Node -> bool
        abstract isComputedPropertyName: node: Node -> bool
        abstract isTypeParameterDeclaration: node: Node -> bool
        abstract isParameter: node: Node -> bool
        abstract isDecorator: node: Node -> bool
        abstract isPropertySignature: node: Node -> bool
        abstract isPropertyDeclaration: node: Node -> bool
        abstract isMethodSignature: node: Node -> bool
        abstract isMethodDeclaration: node: Node -> bool
        abstract isClassStaticBlockDeclaration: node: Node -> bool
        abstract isConstructorDeclaration: node: Node -> bool
        abstract isGetAccessorDeclaration: node: Node -> bool
        abstract isSetAccessorDeclaration: node: Node -> bool
        abstract isCallSignatureDeclaration: node: Node -> bool
        abstract isConstructSignatureDeclaration: node: Node -> bool
        abstract isIndexSignatureDeclaration: node: Node -> bool
        abstract isTypePredicateNode: node: Node -> bool
        abstract isTypeReferenceNode: node: Node -> bool
        abstract isFunctionTypeNode: node: Node -> bool
        abstract isConstructorTypeNode: node: Node -> bool
        abstract isTypeQueryNode: node: Node -> bool
        abstract isTypeLiteralNode: node: Node -> bool
        abstract isArrayTypeNode: node: Node -> bool
        abstract isTupleTypeNode: node: Node -> bool
        abstract isNamedTupleMember: node: Node -> bool
        abstract isOptionalTypeNode: node: Node -> bool
        abstract isRestTypeNode: node: Node -> bool
        abstract isUnionTypeNode: node: Node -> bool
        abstract isIntersectionTypeNode: node: Node -> bool
        abstract isConditionalTypeNode: node: Node -> bool
        abstract isInferTypeNode: node: Node -> bool
        abstract isParenthesizedTypeNode: node: Node -> bool
        abstract isThisTypeNode: node: Node -> bool
        abstract isTypeOperatorNode: node: Node -> bool
        abstract isIndexedAccessTypeNode: node: Node -> bool
        abstract isMappedTypeNode: node: Node -> bool
        abstract isLiteralTypeNode: node: Node -> bool
        abstract isImportTypeNode: node: Node -> bool
        abstract isTemplateLiteralTypeSpan: node: Node -> bool
        abstract isTemplateLiteralTypeNode: node: Node -> bool
        abstract isObjectBindingPattern: node: Node -> bool
        abstract isArrayBindingPattern: node: Node -> bool
        abstract isBindingElement: node: Node -> bool
        abstract isArrayLiteralExpression: node: Node -> bool
        abstract isObjectLiteralExpression: node: Node -> bool
        abstract isPropertyAccessExpression: node: Node -> bool
        abstract isElementAccessExpression: node: Node -> bool
        abstract isCallExpression: node: Node -> bool
        abstract isNewExpression: node: Node -> bool
        abstract isTaggedTemplateExpression: node: Node -> bool
        abstract isTypeAssertionExpression: node: Node -> bool
        abstract isParenthesizedExpression: node: Node -> bool
        abstract isFunctionExpression: node: Node -> bool
        abstract isArrowFunction: node: Node -> bool
        abstract isDeleteExpression: node: Node -> bool
        abstract isTypeOfExpression: node: Node -> bool
        abstract isVoidExpression: node: Node -> bool
        abstract isAwaitExpression: node: Node -> bool
        abstract isPrefixUnaryExpression: node: Node -> bool
        abstract isPostfixUnaryExpression: node: Node -> bool
        abstract isBinaryExpression: node: Node -> bool
        abstract isConditionalExpression: node: Node -> bool
        abstract isTemplateExpression: node: Node -> bool
        abstract isYieldExpression: node: Node -> bool
        abstract isSpreadElement: node: Node -> bool
        abstract isClassExpression: node: Node -> bool
        abstract isOmittedExpression: node: Node -> bool
        abstract isExpressionWithTypeArguments: node: Node -> bool
        abstract isAsExpression: node: Node -> bool
        abstract isNonNullExpression: node: Node -> bool
        abstract isMetaProperty: node: Node -> bool
        abstract isSyntheticExpression: node: Node -> bool
        abstract isPartiallyEmittedExpression: node: Node -> bool
        abstract isCommaListExpression: node: Node -> bool
        abstract isTemplateSpan: node: Node -> bool
        abstract isSemicolonClassElement: node: Node -> bool
        abstract isBlock: node: Node -> bool
        abstract isVariableStatement: node: Node -> bool
        abstract isEmptyStatement: node: Node -> bool
        abstract isExpressionStatement: node: Node -> bool
        abstract isIfStatement: node: Node -> bool
        abstract isDoStatement: node: Node -> bool
        abstract isWhileStatement: node: Node -> bool
        abstract isForStatement: node: Node -> bool
        abstract isForInStatement: node: Node -> bool
        abstract isForOfStatement: node: Node -> bool
        abstract isContinueStatement: node: Node -> bool
        abstract isBreakStatement: node: Node -> bool
        abstract isReturnStatement: node: Node -> bool
        abstract isWithStatement: node: Node -> bool
        abstract isSwitchStatement: node: Node -> bool
        abstract isLabeledStatement: node: Node -> bool
        abstract isThrowStatement: node: Node -> bool
        abstract isTryStatement: node: Node -> bool
        abstract isDebuggerStatement: node: Node -> bool
        abstract isVariableDeclaration: node: Node -> bool
        abstract isVariableDeclarationList: node: Node -> bool
        abstract isFunctionDeclaration: node: Node -> bool
        abstract isClassDeclaration: node: Node -> bool
        abstract isInterfaceDeclaration: node: Node -> bool
        abstract isTypeAliasDeclaration: node: Node -> bool
        abstract isEnumDeclaration: node: Node -> bool
        abstract isModuleDeclaration: node: Node -> bool
        abstract isModuleBlock: node: Node -> bool
        abstract isCaseBlock: node: Node -> bool
        abstract isNamespaceExportDeclaration: node: Node -> bool
        abstract isImportEqualsDeclaration: node: Node -> bool
        abstract isImportDeclaration: node: Node -> bool
        abstract isImportClause: node: Node -> bool
        abstract isAssertClause: node: Node -> bool
        abstract isAssertEntry: node: Node -> bool
        abstract isNamespaceImport: node: Node -> bool
        abstract isNamespaceExport: node: Node -> bool
        abstract isNamedImports: node: Node -> bool
        abstract isImportSpecifier: node: Node -> bool
        abstract isExportAssignment: node: Node -> bool
        abstract isExportDeclaration: node: Node -> bool
        abstract isNamedExports: node: Node -> bool
        abstract isExportSpecifier: node: Node -> bool
        abstract isMissingDeclaration: node: Node -> bool
        abstract isNotEmittedStatement: node: Node -> bool
        abstract isExternalModuleReference: node: Node -> bool
        abstract isJsxElement: node: Node -> bool
        abstract isJsxSelfClosingElement: node: Node -> bool
        abstract isJsxOpeningElement: node: Node -> bool
        abstract isJsxClosingElement: node: Node -> bool
        abstract isJsxFragment: node: Node -> bool
        abstract isJsxOpeningFragment: node: Node -> bool
        abstract isJsxClosingFragment: node: Node -> bool
        abstract isJsxAttribute: node: Node -> bool
        abstract isJsxAttributes: node: Node -> bool
        abstract isJsxSpreadAttribute: node: Node -> bool
        abstract isJsxExpression: node: Node -> bool
        abstract isCaseClause: node: Node -> bool
        abstract isDefaultClause: node: Node -> bool
        abstract isHeritageClause: node: Node -> bool
        abstract isCatchClause: node: Node -> bool
        abstract isPropertyAssignment: node: Node -> bool
        abstract isShorthandPropertyAssignment: node: Node -> bool
        abstract isSpreadAssignment: node: Node -> bool
        abstract isEnumMember: node: Node -> bool
        abstract isUnparsedPrepend: node: Node -> bool
        abstract isSourceFile: node: Node -> bool
        abstract isBundle: node: Node -> bool
        abstract isUnparsedSource: node: Node -> bool
        abstract isJSDocTypeExpression: node: Node -> bool
        abstract isJSDocNameReference: node: Node -> bool
        abstract isJSDocMemberName: node: Node -> bool
        abstract isJSDocLink: node: Node -> bool
        abstract isJSDocLinkCode: node: Node -> bool
        abstract isJSDocLinkPlain: node: Node -> bool
        abstract isJSDocAllType: node: Node -> bool
        abstract isJSDocUnknownType: node: Node -> bool
        abstract isJSDocNullableType: node: Node -> bool
        abstract isJSDocNonNullableType: node: Node -> bool
        abstract isJSDocOptionalType: node: Node -> bool
        abstract isJSDocFunctionType: node: Node -> bool
        abstract isJSDocVariadicType: node: Node -> bool
        abstract isJSDocNamepathType: node: Node -> bool
        abstract isJSDoc: node: Node -> bool
        abstract isJSDocTypeLiteral: node: Node -> bool
        abstract isJSDocSignature: node: Node -> bool
        abstract isJSDocAugmentsTag: node: Node -> bool
        abstract isJSDocAuthorTag: node: Node -> bool
        abstract isJSDocClassTag: node: Node -> bool
        abstract isJSDocCallbackTag: node: Node -> bool
        abstract isJSDocPublicTag: node: Node -> bool
        abstract isJSDocPrivateTag: node: Node -> bool
        abstract isJSDocProtectedTag: node: Node -> bool
        abstract isJSDocReadonlyTag: node: Node -> bool
        abstract isJSDocOverrideTag: node: Node -> bool
        abstract isJSDocDeprecatedTag: node: Node -> bool
        abstract isJSDocSeeTag: node: Node -> bool
        abstract isJSDocEnumTag: node: Node -> bool
        abstract isJSDocParameterTag: node: Node -> bool
        abstract isJSDocReturnTag: node: Node -> bool
        abstract isJSDocThisTag: node: Node -> bool
        abstract isJSDocTypeTag: node: Node -> bool
        abstract isJSDocTemplateTag: node: Node -> bool
        abstract isJSDocTypedefTag: node: Node -> bool
        abstract isJSDocUnknownTag: node: Node -> bool
        abstract isJSDocPropertyTag: node: Node -> bool
        abstract isJSDocImplementsTag: node: Node -> bool
        abstract setTextRange: range: 'T * location: TextRange option -> 'T
        /// <summary>
        /// Invokes a callback for each child of the given node. The 'cbNode' callback is invoked for all child nodes
        /// stored in properties. If a 'cbNodes' callback is specified, it is invoked for embedded arrays; otherwise,
        /// embedded arrays are flattened and the 'cbNode' callback is invoked for each element. If a callback returns
        /// a truthy value, iteration stops and that value is returned. Otherwise, undefined is returned.
        /// </summary>
        /// <param name="node">a given node to visit its children</param>
        /// <param name="cbNode">a callback to be invoked for all child nodes</param>
        /// <param name="cbNodes">a callback to be invoked for embedded array</param>
        /// <remarks>
        /// <c>forEachChild</c> must visit the children of a node in the order
        /// that they appear in the source code. The language service depends on this property to locate nodes by position.
        /// </remarks>
        abstract forEachChild: node: Node * cbNode: (Node -> 'T option) * ?cbNodes: (Node[] -> 'T option) -> 'T option
        abstract createSourceFile: fileName: string * sourceText: string * languageVersion: ScriptTarget * ?setParentNodes: bool * ?scriptKind: ScriptKind -> SourceFile
        abstract parseIsolatedEntityName: text: string * languageVersion: ScriptTarget -> EntityName option
        /// <summary>Parse json text into SyntaxTree and return node and parse errors if any</summary>
        /// <param name="fileName" />
        /// <param name="sourceText" />
        abstract parseJsonText: fileName: string * sourceText: string -> JsonSourceFile
        abstract isExternalModule: file: SourceFile -> bool
        abstract updateSourceFile: sourceFile: SourceFile * newText: string * textChangeRange: TextChangeRange * ?aggressiveChecks: bool -> SourceFile
        abstract parseCommandLine: commandLine: string[] * ?readFile: (string -> string option) -> ParsedCommandLine
        /// Reads the config file, reports errors if any and exits if the config file cannot be found
        abstract getParsedCommandLineOfConfigFile: configFileName: string * optionsToExtend: CompilerOptions option * host: ParseConfigFileHost * ?extendedConfigCache: Map<ExtendedConfigCacheEntry> * ?watchOptionsToExtend: WatchOptions * ?extraFileExtensions: FileExtensionInfo[] -> ParsedCommandLine option
        /// <summary>Read tsconfig.json file</summary>
        /// <param name="fileName">The path to the config file</param>
        abstract readConfigFile: fileName: string * readFile: (string -> string option) -> {| config: obj option; error: Diagnostic option |}
        /// <summary>Parse the text of the tsconfig.json file</summary>
        /// <param name="fileName">The path to the config file</param>
        /// <param name="jsonText">The text of the config file</param>
        abstract parseConfigFileTextToJson: fileName: string * jsonText: string -> {| config: obj option; error: Diagnostic option |}
        /// <summary>Read tsconfig.json file</summary>
        /// <param name="fileName">The path to the config file</param>
        abstract readJsonConfigFile: fileName: string * readFile: (string -> string option) -> TsConfigSourceFile
        /// Convert the json syntax tree into the json value
        abstract convertToObject: sourceFile: JsonSourceFile * errors: Push<Diagnostic> -> obj option
        /// <summary>Parse the contents of a config file (tsconfig.json).</summary>
        /// <param name="json">The contents of the config file to parse</param>
        /// <param name="host">Instance of ParseConfigHost used to enumerate files in folder.</param>
        /// <param name="basePath">
        /// A root directory to resolve relative path entries in the config
        /// file to. e.g. outDir
        /// </param>
        abstract parseJsonConfigFileContent: json: obj option * host: ParseConfigHost * basePath: string * ?existingOptions: CompilerOptions * ?configFileName: string * ?resolutionStack: Path[] * ?extraFileExtensions: FileExtensionInfo[] * ?extendedConfigCache: Map<ExtendedConfigCacheEntry> * ?existingWatchOptions: WatchOptions -> ParsedCommandLine
        /// <summary>Parse the contents of a config file (tsconfig.json).</summary>
        /// <param name="jsonNode">The contents of the config file to parse</param>
        /// <param name="host">Instance of ParseConfigHost used to enumerate files in folder.</param>
        /// <param name="basePath">
        /// A root directory to resolve relative path entries in the config
        /// file to. e.g. outDir
        /// </param>
        abstract parseJsonSourceFileConfigFileContent: sourceFile: TsConfigSourceFile * host: ParseConfigHost * basePath: string * ?existingOptions: CompilerOptions * ?configFileName: string * ?resolutionStack: Path[] * ?extraFileExtensions: FileExtensionInfo[] * ?extendedConfigCache: Map<ExtendedConfigCacheEntry> * ?existingWatchOptions: WatchOptions -> ParsedCommandLine
        abstract convertCompilerOptionsFromJson: jsonOptions: obj option * basePath: string * ?configFileName: string -> {| options: CompilerOptions; errors: Diagnostic[] |}
        abstract convertTypeAcquisitionFromJson: jsonOptions: obj option * basePath: string * ?configFileName: string -> {| options: TypeAcquisition; errors: Diagnostic[] |}
        abstract getEffectiveTypeRoots: options: CompilerOptions * host: GetEffectiveTypeRootsHost -> string[] option
        /// <param name="containingFile">
        /// file that contains type reference directive, can be undefined if containing file is unknown.
        /// This is possible in case if resolution is performed for directives specified via 'types' parameter. In this case initial path for secondary lookups
        /// is assumed to be the same as root directory of the project.
        /// </param>
        abstract resolveTypeReferenceDirective: typeReferenceDirectiveName: string * containingFile: string option * options: CompilerOptions * host: ModuleResolutionHost * ?redirectedReference: ResolvedProjectReference * ?cache: TypeReferenceDirectiveResolutionCache -> ResolvedTypeReferenceDirectiveWithFailedLookupLocations
        /// Given a set of options, returns the set of type directive names
        ///    that should be included for this program automatically.
        /// This list could either come from the config file,
        ///    or from enumerating the types root + initial secondary types lookup location.
        /// More type directives might appear in the program later as a result of loading actual source files;
        ///    this list is only the set of defaults that are implicitly included.
        abstract getAutomaticTypeDirectiveNames: options: CompilerOptions * host: ModuleResolutionHost -> string[]
        abstract createModuleResolutionCache: currentDirectory: string * getCanonicalFileName: (string -> string) * ?options: CompilerOptions -> ModuleResolutionCache
        abstract createTypeReferenceDirectiveResolutionCache: currentDirectory: string * getCanonicalFileName: (string -> string) * ?options: CompilerOptions * ?packageJsonInfoCache: PackageJsonInfoCache -> TypeReferenceDirectiveResolutionCache
        abstract resolveModuleNameFromCache: moduleName: string * containingFile: string * cache: ModuleResolutionCache * ?mode: ModuleKind -> ResolvedModuleWithFailedLookupLocations option
        abstract resolveModuleName: moduleName: string * containingFile: string * compilerOptions: CompilerOptions * host: ModuleResolutionHost * ?cache: ModuleResolutionCache * ?redirectedReference: ResolvedProjectReference * ?resolutionMode: ModuleKind -> ResolvedModuleWithFailedLookupLocations
        abstract nodeModuleNameResolver: moduleName: string * containingFile: string * compilerOptions: CompilerOptions * host: ModuleResolutionHost * ?cache: ModuleResolutionCache * ?redirectedReference: ResolvedProjectReference -> ResolvedModuleWithFailedLookupLocations
        abstract classicNameResolver: moduleName: string * containingFile: string * compilerOptions: CompilerOptions * host: ModuleResolutionHost * ?cache: NonRelativeModuleNameResolutionCache * ?redirectedReference: ResolvedProjectReference -> ResolvedModuleWithFailedLookupLocations
        /// <summary>Visits a Node using the supplied visitor, possibly returning a new Node in its place.</summary>
        /// <param name="node">The Node to visit.</param>
        /// <param name="visitor">The callback used to visit the Node.</param>
        /// <param name="test">A callback to execute to verify the Node is valid.</param>
        /// <param name="lift">An optional callback to execute to lift a NodeArray into a valid Node.</param>
        abstract visitNode: node: 'T * visitor: Visitor option * ?test: (Node -> bool) * ?lift: (Node[] -> 'T) -> 'T
        /// <summary>Visits a Node using the supplied visitor, possibly returning a new Node in its place.</summary>
        /// <param name="node">The Node to visit.</param>
        /// <param name="visitor">The callback used to visit the Node.</param>
        /// <param name="test">A callback to execute to verify the Node is valid.</param>
        /// <param name="lift">An optional callback to execute to lift a NodeArray into a valid Node.</param>
        abstract visitNode: node: 'T option * visitor: Visitor option * ?test: (Node -> bool) * ?lift: (Node[] -> 'T) -> 'T option
        /// <summary>Visits a NodeArray using the supplied visitor, possibly returning a new NodeArray in its place.</summary>
        /// <param name="nodes">The NodeArray to visit.</param>
        /// <param name="visitor">The callback used to visit a Node.</param>
        /// <param name="test">A node test to execute for each node.</param>
        /// <param name="start">An optional value indicating the starting offset at which to start visiting.</param>
        /// <param name="count">An optional value indicating the maximum number of nodes to visit.</param>
        abstract visitNodes: nodes: 'T[] * visitor: Visitor option * ?test: (Node -> bool) * ?start: float * ?count: float -> 'T[]
        /// <summary>Visits a NodeArray using the supplied visitor, possibly returning a new NodeArray in its place.</summary>
        /// <param name="nodes">The NodeArray to visit.</param>
        /// <param name="visitor">The callback used to visit a Node.</param>
        /// <param name="test">A node test to execute for each node.</param>
        /// <param name="start">An optional value indicating the starting offset at which to start visiting.</param>
        /// <param name="count">An optional value indicating the maximum number of nodes to visit.</param>
        abstract visitNodes: nodes: 'T[] option * visitor: Visitor option * ?test: (Node -> bool) * ?start: float * ?count: float -> 'T[] option
        /// Starts a new lexical environment and visits a statement list, ending the lexical environment
        /// and merging hoisted declarations upon completion.
        abstract visitLexicalEnvironment: statements: Statement[] * visitor: Visitor * context: TransformationContext * ?start: float * ?ensureUseStrict: bool * ?nodesVisitor: NodesVisitor -> Statement[]
        /// Starts a new lexical environment and visits a parameter list, suspending the lexical
        /// environment upon completion.
        abstract visitParameterList: nodes: ParameterDeclaration[] * visitor: Visitor * context: TransformationContext * ?nodesVisitor: NodesVisitor -> ParameterDeclaration[]
        abstract visitParameterList: nodes: ParameterDeclaration[] option * visitor: Visitor * context: TransformationContext * ?nodesVisitor: NodesVisitor -> ParameterDeclaration[] option
        /// Resumes a suspended lexical environment and visits a function body, ending the lexical
        /// environment and merging hoisted declarations upon completion.
        abstract visitFunctionBody: node: FunctionBody * visitor: Visitor * context: TransformationContext -> FunctionBody
        /// Resumes a suspended lexical environment and visits a function body, ending the lexical
        /// environment and merging hoisted declarations upon completion.
        abstract visitFunctionBody: node: FunctionBody option * visitor: Visitor * context: TransformationContext -> FunctionBody option
        /// Resumes a suspended lexical environment and visits a concise body, ending the lexical
        /// environment and merging hoisted declarations upon completion.
        abstract visitFunctionBody: node: ConciseBody * visitor: Visitor * context: TransformationContext -> ConciseBody
        /// Visits an iteration body, adding any block-scoped variables required by the transformation.
        abstract visitIterationBody: body: Statement * visitor: Visitor * context: TransformationContext -> Statement
        /// <summary>Visits each child of a Node using the supplied visitor, possibly returning a new Node of the same kind in its place.</summary>
        /// <param name="node">The Node whose children will be visited.</param>
        /// <param name="visitor">The callback used to visit each child.</param>
        /// <param name="context">A lexical environment context for the visitor.</param>
        abstract visitEachChild: node: 'T * visitor: Visitor * context: TransformationContext -> 'T
        /// <summary>Visits each child of a Node using the supplied visitor, possibly returning a new Node of the same kind in its place.</summary>
        /// <param name="node">The Node whose children will be visited.</param>
        /// <param name="visitor">The callback used to visit each child.</param>
        /// <param name="context">A lexical environment context for the visitor.</param>
        abstract visitEachChild: node: 'T option * visitor: Visitor * context: TransformationContext * ?nodesVisitor: obj * ?tokenVisitor: Visitor -> 'T option
        abstract getTsBuildInfoEmitOutputFilePath: options: CompilerOptions -> string option
        abstract getOutputFileNames: commandLine: ParsedCommandLine * inputFileName: string * ignoreCase: bool -> string[]
        abstract createPrinter: ?printerOptions: PrinterOptions * ?handlers: PrintHandlers -> Printer
        abstract findConfigFile: searchPath: string * fileExists: (string -> bool) * ?configName: string -> string option
        abstract resolveTripleslashReference: moduleName: string * containingFile: string -> string
        abstract createCompilerHost: options: CompilerOptions * ?setParentNodes: bool -> CompilerHost
        abstract getPreEmitDiagnostics: program: Program * ?sourceFile: SourceFile * ?cancellationToken: CancellationToken -> Diagnostic[]
        abstract formatDiagnostics: diagnostics: Diagnostic[] * host: FormatDiagnosticsHost -> string
        abstract formatDiagnostic: diagnostic: Diagnostic * host: FormatDiagnosticsHost -> string
        abstract formatDiagnosticsWithColorAndContext: diagnostics: Diagnostic[] * host: FormatDiagnosticsHost -> string
        abstract flattenDiagnosticMessageText: diag: U2<string, DiagnosticMessageChain> option * newLine: string * ?indent: float -> string
        abstract getConfigFileParsingDiagnostics: configFileParseResult: ParsedCommandLine -> Diagnostic[]
        /// <summary>
        /// A function for determining if a given file is esm or cjs format, assuming modern node module resolution rules, as configured by the
        /// <c>options</c> parameter.
        /// </summary>
        /// <param name="fileName">The normalized absolute path to check the format of (it need not exist on disk)</param>
        /// <param name="packageJsonInfoCache">A cache for package file lookups - it's best to have a cache when this function is called often</param>
        /// <param name="host">The ModuleResolutionHost which can perform the filesystem lookups for package json data</param>
        /// <param name="options">The compiler options to perform the analysis under - relevant options are <c>moduleResolution</c> and <c>traceResolution</c></param>
        /// <returns><c>undefined</c> if the path has no relevant implied format, <c>ModuleKind.ESNext</c> for esm format, and <c>ModuleKind.CommonJS</c> for cjs format</returns>
        abstract getImpliedNodeFormatForFile: fileName: Path * packageJsonInfoCache: PackageJsonInfoCache option * host: ModuleResolutionHost * options: CompilerOptions -> ModuleKind option
        /// <summary>
        /// Create a new 'Program' instance. A Program is an immutable collection of 'SourceFile's and a 'CompilerOptions'
        /// that represent a compilation unit.
        ///
        /// Creating a program proceeds from a set of root files, expanding the set of inputs by following imports and
        /// triple-slash-reference-path directives transitively. '@types' and triple-slash-reference-types are also pulled in.
        /// </summary>
        /// <param name="createProgramOptions">The options for creating a program.</param>
        /// <returns>A 'Program' object.</returns>
        abstract createProgram: createProgramOptions: CreateProgramOptions -> Program
        /// <summary>
        /// Create a new 'Program' instance. A Program is an immutable collection of 'SourceFile's and a 'CompilerOptions'
        /// that represent a compilation unit.
        ///
        /// Creating a program proceeds from a set of root files, expanding the set of inputs by following imports and
        /// triple-slash-reference-path directives transitively. '@types' and triple-slash-reference-types are also pulled in.
        /// </summary>
        /// <param name="rootNames">A set of root files.</param>
        /// <param name="options">The compiler options which should be used.</param>
        /// <param name="host">The host interacts with the underlying file system.</param>
        /// <param name="oldProgram">Reuses an old program structure.</param>
        /// <param name="configFileParsingDiagnostics">error during config file parsing</param>
        /// <returns>A 'Program' object.</returns>
        abstract createProgram: rootNames: string[] * options: CompilerOptions * ?host: CompilerHost * ?oldProgram: Program * ?configFileParsingDiagnostics: Diagnostic[] -> Program
        /// Returns the target config filename of a project reference.
        /// Note: The file might not exist.
        abstract resolveProjectReferencePath: ref: ProjectReference -> ResolvedConfigFileName
        [<Obsolete("")>]
        abstract resolveProjectReferencePath: host: ResolveProjectReferencePathHost * ref: ProjectReference -> ResolvedConfigFileName
        /// Create the builder to manage semantic diagnostics and cache them
        abstract createSemanticDiagnosticsBuilderProgram: newProgram: Program * host: BuilderProgramHost * ?oldProgram: SemanticDiagnosticsBuilderProgram * ?configFileParsingDiagnostics: Diagnostic[] -> SemanticDiagnosticsBuilderProgram
        abstract createSemanticDiagnosticsBuilderProgram: rootNames: string[] option * options: CompilerOptions option * ?host: CompilerHost * ?oldProgram: SemanticDiagnosticsBuilderProgram * ?configFileParsingDiagnostics: Diagnostic[] * ?projectReferences: ProjectReference[] -> SemanticDiagnosticsBuilderProgram
        /// Create the builder that can handle the changes in program and iterate through changed files
        /// to emit the those files and manage semantic diagnostics cache as well
        abstract createEmitAndSemanticDiagnosticsBuilderProgram: newProgram: Program * host: BuilderProgramHost * ?oldProgram: EmitAndSemanticDiagnosticsBuilderProgram * ?configFileParsingDiagnostics: Diagnostic[] -> EmitAndSemanticDiagnosticsBuilderProgram
        abstract createEmitAndSemanticDiagnosticsBuilderProgram: rootNames: string[] option * options: CompilerOptions option * ?host: CompilerHost * ?oldProgram: EmitAndSemanticDiagnosticsBuilderProgram * ?configFileParsingDiagnostics: Diagnostic[] * ?projectReferences: ProjectReference[] -> EmitAndSemanticDiagnosticsBuilderProgram
        /// Creates a builder thats just abstraction over program and can be used with watch
        abstract createAbstractBuilder: newProgram: Program * host: BuilderProgramHost * ?oldProgram: BuilderProgram * ?configFileParsingDiagnostics: Diagnostic[] -> BuilderProgram
        abstract createAbstractBuilder: rootNames: string[] option * options: CompilerOptions option * ?host: CompilerHost * ?oldProgram: BuilderProgram * ?configFileParsingDiagnostics: Diagnostic[] * ?projectReferences: ProjectReference[] -> BuilderProgram
        abstract readBuilderProgram: compilerOptions: CompilerOptions * host: ReadBuildProgramHost -> EmitAndSemanticDiagnosticsBuilderProgram option
        abstract createIncrementalCompilerHost: options: CompilerOptions * ?system: System -> CompilerHost
        abstract createIncrementalProgram: p0: IncrementalProgramOptions<'T> -> 'T
        /// Create the watch compiler host for either configFile or fileNames and its options
        abstract createWatchCompilerHost: configFileName: string * optionsToExtend: CompilerOptions option * system: System * ?createProgram: CreateProgram<'T> * ?reportDiagnostic: DiagnosticReporter * ?reportWatchStatus: WatchStatusReporter * ?watchOptionsToExtend: WatchOptions * ?extraFileExtensions: FileExtensionInfo[] -> WatchCompilerHostOfConfigFile<'T>
        abstract createWatchCompilerHost: rootFiles: string[] * options: CompilerOptions * system: System * ?createProgram: CreateProgram<'T> * ?reportDiagnostic: DiagnosticReporter * ?reportWatchStatus: WatchStatusReporter * ?projectReferences: ProjectReference[] * ?watchOptions: WatchOptions -> WatchCompilerHostOfFilesAndCompilerOptions<'T>
        /// Creates the watch from the host for root files and compiler options
        abstract createWatchProgram: host: WatchCompilerHostOfFilesAndCompilerOptions<'T> -> WatchOfFilesAndCompilerOptions<'T>
        /// Creates the watch from the host for config file
        abstract createWatchProgram: host: WatchCompilerHostOfConfigFile<'T> -> WatchOfConfigFile<'T>
        /// Create a function that reports watch status by writing to the system and handles the formating of the diagnostic
        abstract createBuilderStatusReporter: system: System * ?pretty: bool -> DiagnosticReporter
        abstract createSolutionBuilderHost: ?system: System * ?createProgram: CreateProgram<'T> * ?reportDiagnostic: DiagnosticReporter * ?reportSolutionBuilderStatus: DiagnosticReporter * ?reportErrorSummary: ReportEmitErrorSummary -> SolutionBuilderHost<'T>
        abstract createSolutionBuilderWithWatchHost: ?system: System * ?createProgram: CreateProgram<'T> * ?reportDiagnostic: DiagnosticReporter * ?reportSolutionBuilderStatus: DiagnosticReporter * ?reportWatchStatus: WatchStatusReporter -> SolutionBuilderWithWatchHost<'T>
        abstract createSolutionBuilder: host: SolutionBuilderHost<'T> * rootNames: string[] * defaultOptions: BuildOptions -> SolutionBuilder<'T>
        abstract createSolutionBuilderWithWatch: host: SolutionBuilderWithWatchHost<'T> * rootNames: string[] * defaultOptions: BuildOptions * ?baseWatchOptions: WatchOptions -> SolutionBuilder<'T>
        abstract getDefaultFormatCodeSettings: ?newLineCharacter: string -> FormatCodeSettings
        /// The classifier is used for syntactic highlighting in editors via the TSServer
        abstract createClassifier: unit -> Classifier
        abstract createDocumentRegistry: ?useCaseSensitiveFileNames: bool * ?currentDirectory: string -> DocumentRegistry
        abstract preProcessFile: sourceText: string * ?readImportFiles: bool * ?detectJavaScriptImports: bool -> PreProcessedFileInfo
        abstract transpileModule: input: string * transpileOptions: TranspileOptions -> TranspileOutput
        abstract transpile: input: string * ?compilerOptions: CompilerOptions * ?fileName: string * ?diagnostics: Diagnostic[] * ?moduleName: string -> string
        /// The version of the language service API
        abstract servicesVersion: obj
        abstract toEditorSettings: options: U2<EditorOptions, EditorSettings> -> EditorSettings
        abstract displayPartsToString: displayParts: SymbolDisplayPart[] option -> string
        abstract getDefaultCompilerOptions: unit -> CompilerOptions
        abstract getSupportedCodeFixes: unit -> string[]
        abstract createLanguageServiceSourceFile: fileName: string * scriptSnapshot: IScriptSnapshot * scriptTarget: ScriptTarget * version: string * setNodeParents: bool * ?scriptKind: ScriptKind -> SourceFile
        abstract updateLanguageServiceSourceFile: sourceFile: SourceFile * scriptSnapshot: IScriptSnapshot * version: string * textChangeRange: TextChangeRange option * ?aggressiveChecks: bool -> SourceFile
        abstract createLanguageService: host: LanguageServiceHost * ?documentRegistry: DocumentRegistry * ?syntaxOnlyOrLanguageServiceMode: U2<bool, LanguageServiceMode> -> LanguageService
        /// Get the path of the default library files (lib.d.ts) as distributed with the typescript
        /// node package.
        /// The functionality is not supported if the ts module is consumed outside of a node module.
        abstract getDefaultLibFilePath: options: CompilerOptions -> string
        /// <summary>Transform one or more nodes using the supplied transformers.</summary>
        /// <param name="source">A single <c>Node</c> or an array of <c>Node</c> objects.</param>
        /// <param name="transformers">An array of <c>TransformerFactory</c> callbacks used to process the transformation.</param>
        /// <param name="compilerOptions">Optional compiler options.</param>
        abstract transform: source: U2<'T, 'T[]> * transformers: TransformerFactory<'T>[] * ?compilerOptions: CompilerOptions -> TransformationResult<'T>
        [<Obsolete("Use `factory.createNodeArray` or the factory supplied by your transformation context instead.")>]
        abstract createNodeArray: ('T[]) option -> (bool) option -> 'T[]
        [<Obsolete("Use `factory.createNumericLiteral` or the factory supplied by your transformation context instead.")>]
        abstract createNumericLiteral: U2<string, float> -> (TokenFlags) option -> NumericLiteral
        [<Obsolete("Use `factory.createBigIntLiteral` or the factory supplied by your transformation context instead.")>]
        abstract createBigIntLiteral: U2<string, PseudoBigInt> -> BigIntLiteral
        [<Obsolete("Use `factory.createStringLiteral` or the factory supplied by your transformation context instead.")>]
        abstract createStringLiteral: {| Invoke: string -> bool option -> StringLiteral; Invoke: string -> bool option -> bool option -> StringLiteral |}
        [<Obsolete("Use `factory.createStringLiteralFromNode` or the factory supplied by your transformation context instead.")>]
        abstract createStringLiteralFromNode: PropertyNameLiteral -> (bool) option -> StringLiteral
        [<Obsolete("Use `factory.createRegularExpressionLiteral` or the factory supplied by your transformation context instead.")>]
        abstract createRegularExpressionLiteral: string -> RegularExpressionLiteral
        [<Obsolete("Use `factory.createLoopVariable` or the factory supplied by your transformation context instead.")>]
        abstract createLoopVariable: (bool) option -> Identifier
        [<Obsolete("Use `factory.createUniqueName` or the factory supplied by your transformation context instead.")>]
        abstract createUniqueName: string -> (GeneratedIdentifierFlags) option -> Identifier
        [<Obsolete("Use `factory.createPrivateIdentifier` or the factory supplied by your transformation context instead.")>]
        abstract createPrivateIdentifier: string -> PrivateIdentifier
        [<Obsolete("Use `factory.createSuper` or the factory supplied by your transformation context instead.")>]
        abstract createSuper: unit -> SuperExpression
        [<Obsolete("Use `factory.createThis` or the factory supplied by your transformation context instead.")>]
        abstract createThis: unit -> ThisExpression
        [<Obsolete("Use `factory.createNull` or the factory supplied by your transformation context instead.")>]
        abstract createNull: unit -> NullLiteral
        [<Obsolete("Use `factory.createTrue` or the factory supplied by your transformation context instead.")>]
        abstract createTrue: unit -> TrueLiteral
        [<Obsolete("Use `factory.createFalse` or the factory supplied by your transformation context instead.")>]
        abstract createFalse: unit -> FalseLiteral
        [<Obsolete("Use `factory.createModifier` or the factory supplied by your transformation context instead.")>]
        abstract createModifier: 'T -> ModifierToken<'T>
        [<Obsolete("Use `factory.createModifiersFromModifierFlags` or the factory supplied by your transformation context instead.")>]
        abstract createModifiersFromModifierFlags: ModifierFlags -> Modifier[] option
        [<Obsolete("Use `factory.createQualifiedName` or the factory supplied by your transformation context instead.")>]
        abstract createQualifiedName: EntityName -> U2<string, Identifier> -> QualifiedName
        [<Obsolete("Use `factory.updateQualifiedName` or the factory supplied by your transformation context instead.")>]
        abstract updateQualifiedName: QualifiedName -> EntityName -> Identifier -> QualifiedName
        [<Obsolete("Use `factory.createComputedPropertyName` or the factory supplied by your transformation context instead.")>]
        abstract createComputedPropertyName: Expression -> ComputedPropertyName
        [<Obsolete("Use `factory.updateComputedPropertyName` or the factory supplied by your transformation context instead.")>]
        abstract updateComputedPropertyName: ComputedPropertyName -> Expression -> ComputedPropertyName
        [<Obsolete("Use `factory.createTypeParameterDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createTypeParameterDeclaration: U2<string, Identifier> -> (TypeNode) option -> (TypeNode) option -> TypeParameterDeclaration
        [<Obsolete("Use `factory.updateTypeParameterDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateTypeParameterDeclaration: TypeParameterDeclaration -> Identifier -> TypeNode option -> TypeNode option -> TypeParameterDeclaration
        [<Obsolete("Use `factory.createParameterDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createParameter: Decorator[] option -> Modifier[] option -> DotDotDotToken option -> U2<string, BindingName> -> (QuestionToken) option -> (TypeNode) option -> (Expression) option -> ParameterDeclaration
        [<Obsolete("Use `factory.updateParameterDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateParameter: ParameterDeclaration -> Decorator[] option -> Modifier[] option -> DotDotDotToken option -> U2<string, BindingName> -> QuestionToken option -> TypeNode option -> Expression option -> ParameterDeclaration
        [<Obsolete("Use `factory.createDecorator` or the factory supplied by your transformation context instead.")>]
        abstract createDecorator: Expression -> Decorator
        [<Obsolete("Use `factory.updateDecorator` or the factory supplied by your transformation context instead.")>]
        abstract updateDecorator: Decorator -> Expression -> Decorator
        [<Obsolete("Use `factory.createPropertyDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createProperty: Decorator[] option -> Modifier[] option -> U2<string, PropertyName> -> U2<QuestionToken, ExclamationToken> option -> TypeNode option -> Expression option -> PropertyDeclaration
        [<Obsolete("Use `factory.updatePropertyDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateProperty: PropertyDeclaration -> Decorator[] option -> Modifier[] option -> U2<string, PropertyName> -> U2<QuestionToken, ExclamationToken> option -> TypeNode option -> Expression option -> PropertyDeclaration
        [<Obsolete("Use `factory.createMethodDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createMethod: Decorator[] option -> Modifier[] option -> AsteriskToken option -> U2<string, PropertyName> -> QuestionToken option -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> Block option -> MethodDeclaration
        [<Obsolete("Use `factory.updateMethodDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateMethod: MethodDeclaration -> Decorator[] option -> Modifier[] option -> AsteriskToken option -> PropertyName -> QuestionToken option -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> Block option -> MethodDeclaration
        [<Obsolete("Use `factory.createConstructorDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createConstructor: Decorator[] option -> Modifier[] option -> ParameterDeclaration[] -> Block option -> ConstructorDeclaration
        [<Obsolete("Use `factory.updateConstructorDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateConstructor: ConstructorDeclaration -> Decorator[] option -> Modifier[] option -> ParameterDeclaration[] -> Block option -> ConstructorDeclaration
        [<Obsolete("Use `factory.createGetAccessorDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createGetAccessor: Decorator[] option -> Modifier[] option -> U2<string, PropertyName> -> ParameterDeclaration[] -> TypeNode option -> Block option -> GetAccessorDeclaration
        [<Obsolete("Use `factory.updateGetAccessorDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateGetAccessor: GetAccessorDeclaration -> Decorator[] option -> Modifier[] option -> PropertyName -> ParameterDeclaration[] -> TypeNode option -> Block option -> GetAccessorDeclaration
        [<Obsolete("Use `factory.createSetAccessorDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createSetAccessor: Decorator[] option -> Modifier[] option -> U2<string, PropertyName> -> ParameterDeclaration[] -> Block option -> SetAccessorDeclaration
        [<Obsolete("Use `factory.updateSetAccessorDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateSetAccessor: SetAccessorDeclaration -> Decorator[] option -> Modifier[] option -> PropertyName -> ParameterDeclaration[] -> Block option -> SetAccessorDeclaration
        [<Obsolete("Use `factory.createCallSignature` or the factory supplied by your transformation context instead.")>]
        abstract createCallSignature: TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> CallSignatureDeclaration
        [<Obsolete("Use `factory.updateCallSignature` or the factory supplied by your transformation context instead.")>]
        abstract updateCallSignature: CallSignatureDeclaration -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> CallSignatureDeclaration
        [<Obsolete("Use `factory.createConstructSignature` or the factory supplied by your transformation context instead.")>]
        abstract createConstructSignature: TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> ConstructSignatureDeclaration
        [<Obsolete("Use `factory.updateConstructSignature` or the factory supplied by your transformation context instead.")>]
        abstract updateConstructSignature: ConstructSignatureDeclaration -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> ConstructSignatureDeclaration
        [<Obsolete("Use `factory.updateIndexSignature` or the factory supplied by your transformation context instead.")>]
        abstract updateIndexSignature: IndexSignatureDeclaration -> Decorator[] option -> Modifier[] option -> ParameterDeclaration[] -> TypeNode -> IndexSignatureDeclaration
        [<Obsolete("Use `factory.createKeywordTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createKeywordTypeNode: 'TKind -> KeywordTypeNode<'TKind>
        [<Obsolete("Use `factory.createTypePredicateNode` or the factory supplied by your transformation context instead.")>]
        abstract createTypePredicateNodeWithModifier: AssertsKeyword option -> U3<string, Identifier, ThisTypeNode> -> TypeNode option -> TypePredicateNode
        [<Obsolete("Use `factory.updateTypePredicateNode` or the factory supplied by your transformation context instead.")>]
        abstract updateTypePredicateNodeWithModifier: TypePredicateNode -> AssertsKeyword option -> U2<Identifier, ThisTypeNode> -> TypeNode option -> TypePredicateNode
        [<Obsolete("Use `factory.createTypeReferenceNode` or the factory supplied by your transformation context instead.")>]
        abstract createTypeReferenceNode: U2<string, EntityName> -> (TypeNode[]) option -> TypeReferenceNode
        [<Obsolete("Use `factory.updateTypeReferenceNode` or the factory supplied by your transformation context instead.")>]
        abstract updateTypeReferenceNode: TypeReferenceNode -> EntityName -> TypeNode[] option -> TypeReferenceNode
        [<Obsolete("Use `factory.createFunctionTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createFunctionTypeNode: TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode -> FunctionTypeNode
        [<Obsolete("Use `factory.updateFunctionTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateFunctionTypeNode: FunctionTypeNode -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode -> FunctionTypeNode
        [<Obsolete("Use `factory.createConstructorTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createConstructorTypeNode: TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode -> ConstructorTypeNode
        [<Obsolete("Use `factory.updateConstructorTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateConstructorTypeNode: ConstructorTypeNode -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode -> ConstructorTypeNode
        [<Obsolete("Use `factory.createTypeQueryNode` or the factory supplied by your transformation context instead.")>]
        abstract createTypeQueryNode: EntityName -> TypeQueryNode
        [<Obsolete("Use `factory.updateTypeQueryNode` or the factory supplied by your transformation context instead.")>]
        abstract updateTypeQueryNode: TypeQueryNode -> EntityName -> TypeQueryNode
        [<Obsolete("Use `factory.createTypeLiteralNode` or the factory supplied by your transformation context instead.")>]
        abstract createTypeLiteralNode: TypeElement[] option -> TypeLiteralNode
        [<Obsolete("Use `factory.updateTypeLiteralNode` or the factory supplied by your transformation context instead.")>]
        abstract updateTypeLiteralNode: TypeLiteralNode -> TypeElement[] -> TypeLiteralNode
        [<Obsolete("Use `factory.createArrayTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createArrayTypeNode: TypeNode -> ArrayTypeNode
        [<Obsolete("Use `factory.updateArrayTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateArrayTypeNode: ArrayTypeNode -> TypeNode -> ArrayTypeNode
        [<Obsolete("Use `factory.createTupleTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createTupleTypeNode: U2<TypeNode, NamedTupleMember>[] -> TupleTypeNode
        [<Obsolete("Use `factory.updateTupleTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateTupleTypeNode: TupleTypeNode -> U2<TypeNode, NamedTupleMember>[] -> TupleTypeNode
        [<Obsolete("Use `factory.createOptionalTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createOptionalTypeNode: TypeNode -> OptionalTypeNode
        [<Obsolete("Use `factory.updateOptionalTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateOptionalTypeNode: OptionalTypeNode -> TypeNode -> OptionalTypeNode
        [<Obsolete("Use `factory.createRestTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createRestTypeNode: TypeNode -> RestTypeNode
        [<Obsolete("Use `factory.updateRestTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateRestTypeNode: RestTypeNode -> TypeNode -> RestTypeNode
        [<Obsolete("Use `factory.createUnionTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createUnionTypeNode: TypeNode[] -> UnionTypeNode
        [<Obsolete("Use `factory.updateUnionTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateUnionTypeNode: UnionTypeNode -> TypeNode[] -> UnionTypeNode
        [<Obsolete("Use `factory.createIntersectionTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createIntersectionTypeNode: TypeNode[] -> IntersectionTypeNode
        [<Obsolete("Use `factory.updateIntersectionTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateIntersectionTypeNode: IntersectionTypeNode -> TypeNode[] -> IntersectionTypeNode
        [<Obsolete("Use `factory.createConditionalTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createConditionalTypeNode: TypeNode -> TypeNode -> TypeNode -> TypeNode -> ConditionalTypeNode
        [<Obsolete("Use `factory.updateConditionalTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateConditionalTypeNode: ConditionalTypeNode -> TypeNode -> TypeNode -> TypeNode -> TypeNode -> ConditionalTypeNode
        [<Obsolete("Use `factory.createInferTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createInferTypeNode: TypeParameterDeclaration -> InferTypeNode
        [<Obsolete("Use `factory.updateInferTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateInferTypeNode: InferTypeNode -> TypeParameterDeclaration -> InferTypeNode
        [<Obsolete("Use `factory.createImportTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createImportTypeNode: TypeNode -> (EntityName) option -> (TypeNode[]) option -> (bool) option -> ImportTypeNode
        [<Obsolete("Use `factory.updateImportTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateImportTypeNode: ImportTypeNode -> TypeNode -> EntityName option -> TypeNode[] option -> (bool) option -> ImportTypeNode
        [<Obsolete("Use `factory.createParenthesizedType` or the factory supplied by your transformation context instead.")>]
        abstract createParenthesizedType: TypeNode -> ParenthesizedTypeNode
        [<Obsolete("Use `factory.updateParenthesizedType` or the factory supplied by your transformation context instead.")>]
        abstract updateParenthesizedType: ParenthesizedTypeNode -> TypeNode -> ParenthesizedTypeNode
        [<Obsolete("Use `factory.createThisTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createThisTypeNode: unit -> ThisTypeNode
        [<Obsolete("Use `factory.updateTypeOperatorNode` or the factory supplied by your transformation context instead.")>]
        abstract updateTypeOperatorNode: TypeOperatorNode -> TypeNode -> TypeOperatorNode
        [<Obsolete("Use `factory.createIndexedAccessTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createIndexedAccessTypeNode: TypeNode -> TypeNode -> IndexedAccessTypeNode
        [<Obsolete("Use `factory.updateIndexedAccessTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateIndexedAccessTypeNode: IndexedAccessTypeNode -> TypeNode -> TypeNode -> IndexedAccessTypeNode
        [<Obsolete("Use `factory.createMappedTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createMappedTypeNode: U3<ReadonlyKeyword, PlusToken, MinusToken> option -> TypeParameterDeclaration -> TypeNode option -> U3<QuestionToken, PlusToken, MinusToken> option -> TypeNode option -> TypeElement[] option -> MappedTypeNode
        [<Obsolete("Use `factory.updateMappedTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateMappedTypeNode: MappedTypeNode -> U3<ReadonlyKeyword, PlusToken, MinusToken> option -> TypeParameterDeclaration -> TypeNode option -> U3<QuestionToken, PlusToken, MinusToken> option -> TypeNode option -> TypeElement[] option -> MappedTypeNode
        [<Obsolete("Use `factory.createLiteralTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract createLiteralTypeNode: U4<LiteralExpression, BooleanLiteral, PrefixUnaryExpression, NullLiteral> -> LiteralTypeNode
        [<Obsolete("Use `factory.updateLiteralTypeNode` or the factory supplied by your transformation context instead.")>]
        abstract updateLiteralTypeNode: LiteralTypeNode -> U4<LiteralExpression, BooleanLiteral, PrefixUnaryExpression, NullLiteral> -> LiteralTypeNode
        [<Obsolete("Use `factory.createObjectBindingPattern` or the factory supplied by your transformation context instead.")>]
        abstract createObjectBindingPattern: BindingElement[] -> ObjectBindingPattern
        [<Obsolete("Use `factory.updateObjectBindingPattern` or the factory supplied by your transformation context instead.")>]
        abstract updateObjectBindingPattern: ObjectBindingPattern -> BindingElement[] -> ObjectBindingPattern
        [<Obsolete("Use `factory.createArrayBindingPattern` or the factory supplied by your transformation context instead.")>]
        abstract createArrayBindingPattern: ArrayBindingElement[] -> ArrayBindingPattern
        [<Obsolete("Use `factory.updateArrayBindingPattern` or the factory supplied by your transformation context instead.")>]
        abstract updateArrayBindingPattern: ArrayBindingPattern -> ArrayBindingElement[] -> ArrayBindingPattern
        [<Obsolete("Use `factory.createBindingElement` or the factory supplied by your transformation context instead.")>]
        abstract createBindingElement: DotDotDotToken option -> U2<string, PropertyName> option -> U2<string, BindingName> -> (Expression) option -> BindingElement
        [<Obsolete("Use `factory.updateBindingElement` or the factory supplied by your transformation context instead.")>]
        abstract updateBindingElement: BindingElement -> DotDotDotToken option -> PropertyName option -> BindingName -> Expression option -> BindingElement
        [<Obsolete("Use `factory.createArrayLiteralExpression` or the factory supplied by your transformation context instead.")>]
        abstract createArrayLiteral: (Expression[]) option -> (bool) option -> ArrayLiteralExpression
        [<Obsolete("Use `factory.updateArrayLiteralExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateArrayLiteral: ArrayLiteralExpression -> Expression[] -> ArrayLiteralExpression
        [<Obsolete("Use `factory.createObjectLiteralExpression` or the factory supplied by your transformation context instead.")>]
        abstract createObjectLiteral: (ObjectLiteralElementLike[]) option -> (bool) option -> ObjectLiteralExpression
        [<Obsolete("Use `factory.updateObjectLiteralExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateObjectLiteral: ObjectLiteralExpression -> ObjectLiteralElementLike[] -> ObjectLiteralExpression
        [<Obsolete("Use `factory.createPropertyAccessExpression` or the factory supplied by your transformation context instead.")>]
        abstract createPropertyAccess: Expression -> U2<string, MemberName> -> PropertyAccessExpression
        [<Obsolete("Use `factory.updatePropertyAccessExpression` or the factory supplied by your transformation context instead.")>]
        abstract updatePropertyAccess: PropertyAccessExpression -> Expression -> MemberName -> PropertyAccessExpression
        [<Obsolete("Use `factory.createPropertyAccessChain` or the factory supplied by your transformation context instead.")>]
        abstract createPropertyAccessChain: Expression -> QuestionDotToken option -> U2<string, MemberName> -> PropertyAccessChain
        [<Obsolete("Use `factory.updatePropertyAccessChain` or the factory supplied by your transformation context instead.")>]
        abstract updatePropertyAccessChain: PropertyAccessChain -> Expression -> QuestionDotToken option -> MemberName -> PropertyAccessChain
        [<Obsolete("Use `factory.createElementAccessExpression` or the factory supplied by your transformation context instead.")>]
        abstract createElementAccess: Expression -> U2<float, Expression> -> ElementAccessExpression
        [<Obsolete("Use `factory.updateElementAccessExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateElementAccess: ElementAccessExpression -> Expression -> Expression -> ElementAccessExpression
        [<Obsolete("Use `factory.createElementAccessChain` or the factory supplied by your transformation context instead.")>]
        abstract createElementAccessChain: Expression -> QuestionDotToken option -> U2<float, Expression> -> ElementAccessChain
        [<Obsolete("Use `factory.updateElementAccessChain` or the factory supplied by your transformation context instead.")>]
        abstract updateElementAccessChain: ElementAccessChain -> Expression -> QuestionDotToken option -> Expression -> ElementAccessChain
        [<Obsolete("Use `factory.createCallExpression` or the factory supplied by your transformation context instead.")>]
        abstract createCall: Expression -> TypeNode[] option -> Expression[] option -> CallExpression
        [<Obsolete("Use `factory.updateCallExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateCall: CallExpression -> Expression -> TypeNode[] option -> Expression[] -> CallExpression
        [<Obsolete("Use `factory.createCallChain` or the factory supplied by your transformation context instead.")>]
        abstract createCallChain: Expression -> QuestionDotToken option -> TypeNode[] option -> Expression[] option -> CallChain
        [<Obsolete("Use `factory.updateCallChain` or the factory supplied by your transformation context instead.")>]
        abstract updateCallChain: CallChain -> Expression -> QuestionDotToken option -> TypeNode[] option -> Expression[] -> CallChain
        [<Obsolete("Use `factory.createNewExpression` or the factory supplied by your transformation context instead.")>]
        abstract createNew: Expression -> TypeNode[] option -> Expression[] option -> NewExpression
        [<Obsolete("Use `factory.updateNewExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateNew: NewExpression -> Expression -> TypeNode[] option -> Expression[] option -> NewExpression
        [<Obsolete("Use `factory.createTypeAssertion` or the factory supplied by your transformation context instead.")>]
        abstract createTypeAssertion: TypeNode -> Expression -> TypeAssertion
        [<Obsolete("Use `factory.updateTypeAssertion` or the factory supplied by your transformation context instead.")>]
        abstract updateTypeAssertion: TypeAssertion -> TypeNode -> Expression -> TypeAssertion
        [<Obsolete("Use `factory.createParenthesizedExpression` or the factory supplied by your transformation context instead.")>]
        abstract createParen: Expression -> ParenthesizedExpression
        [<Obsolete("Use `factory.updateParenthesizedExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateParen: ParenthesizedExpression -> Expression -> ParenthesizedExpression
        [<Obsolete("Use `factory.createFunctionExpression` or the factory supplied by your transformation context instead.")>]
        abstract createFunctionExpression: Modifier[] option -> AsteriskToken option -> U2<string, Identifier> option -> TypeParameterDeclaration[] option -> ParameterDeclaration[] option -> TypeNode option -> Block -> FunctionExpression
        [<Obsolete("Use `factory.updateFunctionExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateFunctionExpression: FunctionExpression -> Modifier[] option -> AsteriskToken option -> Identifier option -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> Block -> FunctionExpression
        [<Obsolete("Use `factory.createDeleteExpression` or the factory supplied by your transformation context instead.")>]
        abstract createDelete: Expression -> DeleteExpression
        [<Obsolete("Use `factory.updateDeleteExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateDelete: DeleteExpression -> Expression -> DeleteExpression
        [<Obsolete("Use `factory.createTypeOfExpression` or the factory supplied by your transformation context instead.")>]
        abstract createTypeOf: Expression -> TypeOfExpression
        [<Obsolete("Use `factory.updateTypeOfExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateTypeOf: TypeOfExpression -> Expression -> TypeOfExpression
        [<Obsolete("Use `factory.createVoidExpression` or the factory supplied by your transformation context instead.")>]
        abstract createVoid: Expression -> VoidExpression
        [<Obsolete("Use `factory.updateVoidExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateVoid: VoidExpression -> Expression -> VoidExpression
        [<Obsolete("Use `factory.createAwaitExpression` or the factory supplied by your transformation context instead.")>]
        abstract createAwait: Expression -> AwaitExpression
        [<Obsolete("Use `factory.updateAwaitExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateAwait: AwaitExpression -> Expression -> AwaitExpression
        [<Obsolete("Use `factory.createPrefixExpression` or the factory supplied by your transformation context instead.")>]
        abstract createPrefix: PrefixUnaryOperator -> Expression -> PrefixUnaryExpression
        [<Obsolete("Use `factory.updatePrefixExpression` or the factory supplied by your transformation context instead.")>]
        abstract updatePrefix: PrefixUnaryExpression -> Expression -> PrefixUnaryExpression
        [<Obsolete("Use `factory.createPostfixUnaryExpression` or the factory supplied by your transformation context instead.")>]
        abstract createPostfix: Expression -> PostfixUnaryOperator -> PostfixUnaryExpression
        [<Obsolete("Use `factory.updatePostfixUnaryExpression` or the factory supplied by your transformation context instead.")>]
        abstract updatePostfix: PostfixUnaryExpression -> Expression -> PostfixUnaryExpression
        [<Obsolete("Use `factory.createBinaryExpression` or the factory supplied by your transformation context instead.")>]
        abstract createBinary: Expression -> U2<BinaryOperator, BinaryOperatorToken> -> Expression -> BinaryExpression
        [<Obsolete("Use `factory.updateConditionalExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateConditional: ConditionalExpression -> Expression -> QuestionToken -> Expression -> ColonToken -> Expression -> ConditionalExpression
        [<Obsolete("Use `factory.createTemplateExpression` or the factory supplied by your transformation context instead.")>]
        abstract createTemplateExpression: TemplateHead -> TemplateSpan[] -> TemplateExpression
        [<Obsolete("Use `factory.updateTemplateExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateTemplateExpression: TemplateExpression -> TemplateHead -> TemplateSpan[] -> TemplateExpression
        [<Obsolete("Use `factory.createTemplateHead` or the factory supplied by your transformation context instead.")>]
        abstract createTemplateHead: {| Invoke: string -> string option -> TokenFlags option -> TemplateHead; Invoke: string option -> string -> TokenFlags option -> TemplateHead |}
        [<Obsolete("Use `factory.createTemplateMiddle` or the factory supplied by your transformation context instead.")>]
        abstract createTemplateMiddle: {| Invoke: string -> string option -> TokenFlags option -> TemplateMiddle; Invoke: string option -> string -> TokenFlags option -> TemplateMiddle |}
        [<Obsolete("Use `factory.createTemplateTail` or the factory supplied by your transformation context instead.")>]
        abstract createTemplateTail: {| Invoke: string -> string option -> TokenFlags option -> TemplateTail; Invoke: string option -> string -> TokenFlags option -> TemplateTail |}
        [<Obsolete("Use `factory.createNoSubstitutionTemplateLiteral` or the factory supplied by your transformation context instead.")>]
        abstract createNoSubstitutionTemplateLiteral: {| Invoke: string -> string option -> NoSubstitutionTemplateLiteral; Invoke: string option -> string -> NoSubstitutionTemplateLiteral |}
        [<Obsolete("Use `factory.updateYieldExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateYield: YieldExpression -> AsteriskToken option -> Expression option -> YieldExpression
        [<Obsolete("Use `factory.createSpreadExpression` or the factory supplied by your transformation context instead.")>]
        abstract createSpread: Expression -> SpreadElement
        [<Obsolete("Use `factory.updateSpreadExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateSpread: SpreadElement -> Expression -> SpreadElement
        [<Obsolete("Use `factory.createOmittedExpression` or the factory supplied by your transformation context instead.")>]
        abstract createOmittedExpression: unit -> OmittedExpression
        [<Obsolete("Use `factory.createAsExpression` or the factory supplied by your transformation context instead.")>]
        abstract createAsExpression: Expression -> TypeNode -> AsExpression
        [<Obsolete("Use `factory.updateAsExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateAsExpression: AsExpression -> Expression -> TypeNode -> AsExpression
        [<Obsolete("Use `factory.createNonNullExpression` or the factory supplied by your transformation context instead.")>]
        abstract createNonNullExpression: Expression -> NonNullExpression
        [<Obsolete("Use `factory.updateNonNullExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateNonNullExpression: NonNullExpression -> Expression -> NonNullExpression
        [<Obsolete("Use `factory.createNonNullChain` or the factory supplied by your transformation context instead.")>]
        abstract createNonNullChain: Expression -> NonNullChain
        [<Obsolete("Use `factory.updateNonNullChain` or the factory supplied by your transformation context instead.")>]
        abstract updateNonNullChain: NonNullChain -> Expression -> NonNullChain
        [<Obsolete("Use `factory.createMetaProperty` or the factory supplied by your transformation context instead.")>]
        abstract createMetaProperty: SyntaxKind -> Identifier -> MetaProperty
        [<Obsolete("Use `factory.updateMetaProperty` or the factory supplied by your transformation context instead.")>]
        abstract updateMetaProperty: MetaProperty -> Identifier -> MetaProperty
        [<Obsolete("Use `factory.createTemplateSpan` or the factory supplied by your transformation context instead.")>]
        abstract createTemplateSpan: Expression -> U2<TemplateMiddle, TemplateTail> -> TemplateSpan
        [<Obsolete("Use `factory.updateTemplateSpan` or the factory supplied by your transformation context instead.")>]
        abstract updateTemplateSpan: TemplateSpan -> Expression -> U2<TemplateMiddle, TemplateTail> -> TemplateSpan
        [<Obsolete("Use `factory.createSemicolonClassElement` or the factory supplied by your transformation context instead.")>]
        abstract createSemicolonClassElement: unit -> SemicolonClassElement
        [<Obsolete("Use `factory.createBlock` or the factory supplied by your transformation context instead.")>]
        abstract createBlock: Statement[] -> (bool) option -> Block
        [<Obsolete("Use `factory.updateBlock` or the factory supplied by your transformation context instead.")>]
        abstract updateBlock: Block -> Statement[] -> Block
        [<Obsolete("Use `factory.createVariableStatement` or the factory supplied by your transformation context instead.")>]
        abstract createVariableStatement: Modifier[] option -> U2<VariableDeclarationList, VariableDeclaration[]> -> VariableStatement
        [<Obsolete("Use `factory.updateVariableStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateVariableStatement: VariableStatement -> Modifier[] option -> VariableDeclarationList -> VariableStatement
        [<Obsolete("Use `factory.createEmptyStatement` or the factory supplied by your transformation context instead.")>]
        abstract createEmptyStatement: unit -> EmptyStatement
        [<Obsolete("Use `factory.createExpressionStatement` or the factory supplied by your transformation context instead.")>]
        abstract createExpressionStatement: Expression -> ExpressionStatement
        [<Obsolete("Use `factory.updateExpressionStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateExpressionStatement: ExpressionStatement -> Expression -> ExpressionStatement
        [<Obsolete("Use `factory.createExpressionStatement` or the factory supplied by your transformation context instead.")>]
        abstract createStatement: Expression -> ExpressionStatement
        [<Obsolete("Use `factory.updateExpressionStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateStatement: ExpressionStatement -> Expression -> ExpressionStatement
        [<Obsolete("Use `factory.createIfStatement` or the factory supplied by your transformation context instead.")>]
        abstract createIf: Expression -> Statement -> (Statement) option -> IfStatement
        [<Obsolete("Use `factory.updateIfStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateIf: IfStatement -> Expression -> Statement -> Statement option -> IfStatement
        [<Obsolete("Use `factory.createDoStatement` or the factory supplied by your transformation context instead.")>]
        abstract createDo: Statement -> Expression -> DoStatement
        [<Obsolete("Use `factory.updateDoStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateDo: DoStatement -> Statement -> Expression -> DoStatement
        [<Obsolete("Use `factory.createWhileStatement` or the factory supplied by your transformation context instead.")>]
        abstract createWhile: Expression -> Statement -> WhileStatement
        [<Obsolete("Use `factory.updateWhileStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateWhile: WhileStatement -> Expression -> Statement -> WhileStatement
        [<Obsolete("Use `factory.createForStatement` or the factory supplied by your transformation context instead.")>]
        abstract createFor: ForInitializer option -> Expression option -> Expression option -> Statement -> ForStatement
        [<Obsolete("Use `factory.updateForStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateFor: ForStatement -> ForInitializer option -> Expression option -> Expression option -> Statement -> ForStatement
        [<Obsolete("Use `factory.createForInStatement` or the factory supplied by your transformation context instead.")>]
        abstract createForIn: ForInitializer -> Expression -> Statement -> ForInStatement
        [<Obsolete("Use `factory.updateForInStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateForIn: ForInStatement -> ForInitializer -> Expression -> Statement -> ForInStatement
        [<Obsolete("Use `factory.createForOfStatement` or the factory supplied by your transformation context instead.")>]
        abstract createForOf: AwaitKeyword option -> ForInitializer -> Expression -> Statement -> ForOfStatement
        [<Obsolete("Use `factory.updateForOfStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateForOf: ForOfStatement -> AwaitKeyword option -> ForInitializer -> Expression -> Statement -> ForOfStatement
        [<Obsolete("Use `factory.createContinueStatement` or the factory supplied by your transformation context instead.")>]
        abstract createContinue: (U2<string, Identifier>) option -> ContinueStatement
        [<Obsolete("Use `factory.updateContinueStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateContinue: ContinueStatement -> Identifier option -> ContinueStatement
        [<Obsolete("Use `factory.createBreakStatement` or the factory supplied by your transformation context instead.")>]
        abstract createBreak: (U2<string, Identifier>) option -> BreakStatement
        [<Obsolete("Use `factory.updateBreakStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateBreak: BreakStatement -> Identifier option -> BreakStatement
        [<Obsolete("Use `factory.createReturnStatement` or the factory supplied by your transformation context instead.")>]
        abstract createReturn: (Expression) option -> ReturnStatement
        [<Obsolete("Use `factory.updateReturnStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateReturn: ReturnStatement -> Expression option -> ReturnStatement
        [<Obsolete("Use `factory.createWithStatement` or the factory supplied by your transformation context instead.")>]
        abstract createWith: Expression -> Statement -> WithStatement
        [<Obsolete("Use `factory.updateWithStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateWith: WithStatement -> Expression -> Statement -> WithStatement
        [<Obsolete("Use `factory.createSwitchStatement` or the factory supplied by your transformation context instead.")>]
        abstract createSwitch: Expression -> CaseBlock -> SwitchStatement
        [<Obsolete("Use `factory.updateSwitchStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateSwitch: SwitchStatement -> Expression -> CaseBlock -> SwitchStatement
        [<Obsolete("Use `factory.createLabelStatement` or the factory supplied by your transformation context instead.")>]
        abstract createLabel: U2<string, Identifier> -> Statement -> LabeledStatement
        [<Obsolete("Use `factory.updateLabelStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateLabel: LabeledStatement -> Identifier -> Statement -> LabeledStatement
        [<Obsolete("Use `factory.createThrowStatement` or the factory supplied by your transformation context instead.")>]
        abstract createThrow: Expression -> ThrowStatement
        [<Obsolete("Use `factory.updateThrowStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateThrow: ThrowStatement -> Expression -> ThrowStatement
        [<Obsolete("Use `factory.createTryStatement` or the factory supplied by your transformation context instead.")>]
        abstract createTry: Block -> CatchClause option -> Block option -> TryStatement
        [<Obsolete("Use `factory.updateTryStatement` or the factory supplied by your transformation context instead.")>]
        abstract updateTry: TryStatement -> Block -> CatchClause option -> Block option -> TryStatement
        [<Obsolete("Use `factory.createDebuggerStatement` or the factory supplied by your transformation context instead.")>]
        abstract createDebuggerStatement: unit -> DebuggerStatement
        [<Obsolete("Use `factory.createVariableDeclarationList` or the factory supplied by your transformation context instead.")>]
        abstract createVariableDeclarationList: VariableDeclaration[] -> (NodeFlags) option -> VariableDeclarationList
        [<Obsolete("Use `factory.updateVariableDeclarationList` or the factory supplied by your transformation context instead.")>]
        abstract updateVariableDeclarationList: VariableDeclarationList -> VariableDeclaration[] -> VariableDeclarationList
        [<Obsolete("Use `factory.createFunctionDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createFunctionDeclaration: Decorator[] option -> Modifier[] option -> AsteriskToken option -> U2<string, Identifier> option -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> Block option -> FunctionDeclaration
        [<Obsolete("Use `factory.updateFunctionDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateFunctionDeclaration: FunctionDeclaration -> Decorator[] option -> Modifier[] option -> AsteriskToken option -> Identifier option -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> Block option -> FunctionDeclaration
        [<Obsolete("Use `factory.createClassDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createClassDeclaration: Decorator[] option -> Modifier[] option -> U2<string, Identifier> option -> TypeParameterDeclaration[] option -> HeritageClause[] option -> ClassElement[] -> ClassDeclaration
        [<Obsolete("Use `factory.updateClassDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateClassDeclaration: ClassDeclaration -> Decorator[] option -> Modifier[] option -> Identifier option -> TypeParameterDeclaration[] option -> HeritageClause[] option -> ClassElement[] -> ClassDeclaration
        [<Obsolete("Use `factory.createInterfaceDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createInterfaceDeclaration: Decorator[] option -> Modifier[] option -> U2<string, Identifier> -> TypeParameterDeclaration[] option -> HeritageClause[] option -> TypeElement[] -> InterfaceDeclaration
        [<Obsolete("Use `factory.updateInterfaceDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateInterfaceDeclaration: InterfaceDeclaration -> Decorator[] option -> Modifier[] option -> Identifier -> TypeParameterDeclaration[] option -> HeritageClause[] option -> TypeElement[] -> InterfaceDeclaration
        [<Obsolete("Use `factory.createTypeAliasDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createTypeAliasDeclaration: Decorator[] option -> Modifier[] option -> U2<string, Identifier> -> TypeParameterDeclaration[] option -> TypeNode -> TypeAliasDeclaration
        [<Obsolete("Use `factory.updateTypeAliasDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateTypeAliasDeclaration: TypeAliasDeclaration -> Decorator[] option -> Modifier[] option -> Identifier -> TypeParameterDeclaration[] option -> TypeNode -> TypeAliasDeclaration
        [<Obsolete("Use `factory.createEnumDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createEnumDeclaration: Decorator[] option -> Modifier[] option -> U2<string, Identifier> -> EnumMember[] -> EnumDeclaration
        [<Obsolete("Use `factory.updateEnumDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateEnumDeclaration: EnumDeclaration -> Decorator[] option -> Modifier[] option -> Identifier -> EnumMember[] -> EnumDeclaration
        [<Obsolete("Use `factory.createModuleDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createModuleDeclaration: Decorator[] option -> Modifier[] option -> ModuleName -> ModuleBody option -> (NodeFlags) option -> ModuleDeclaration
        [<Obsolete("Use `factory.updateModuleDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateModuleDeclaration: ModuleDeclaration -> Decorator[] option -> Modifier[] option -> ModuleName -> ModuleBody option -> ModuleDeclaration
        [<Obsolete("Use `factory.createModuleBlock` or the factory supplied by your transformation context instead.")>]
        abstract createModuleBlock: Statement[] -> ModuleBlock
        [<Obsolete("Use `factory.updateModuleBlock` or the factory supplied by your transformation context instead.")>]
        abstract updateModuleBlock: ModuleBlock -> Statement[] -> ModuleBlock
        [<Obsolete("Use `factory.createCaseBlock` or the factory supplied by your transformation context instead.")>]
        abstract createCaseBlock: CaseOrDefaultClause[] -> CaseBlock
        [<Obsolete("Use `factory.updateCaseBlock` or the factory supplied by your transformation context instead.")>]
        abstract updateCaseBlock: CaseBlock -> CaseOrDefaultClause[] -> CaseBlock
        [<Obsolete("Use `factory.createNamespaceExportDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createNamespaceExportDeclaration: U2<string, Identifier> -> NamespaceExportDeclaration
        [<Obsolete("Use `factory.updateNamespaceExportDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateNamespaceExportDeclaration: NamespaceExportDeclaration -> Identifier -> NamespaceExportDeclaration
        [<Obsolete("Use `factory.createImportEqualsDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createImportEqualsDeclaration: Decorator[] option -> Modifier[] option -> bool -> U2<string, Identifier> -> ModuleReference -> ImportEqualsDeclaration
        [<Obsolete("Use `factory.updateImportEqualsDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateImportEqualsDeclaration: ImportEqualsDeclaration -> Decorator[] option -> Modifier[] option -> bool -> Identifier -> ModuleReference -> ImportEqualsDeclaration
        [<Obsolete("Use `factory.createImportDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createImportDeclaration: Decorator[] option -> Modifier[] option -> ImportClause option -> Expression -> (AssertClause) option -> ImportDeclaration
        [<Obsolete("Use `factory.updateImportDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateImportDeclaration: ImportDeclaration -> Decorator[] option -> Modifier[] option -> ImportClause option -> Expression -> AssertClause option -> ImportDeclaration
        [<Obsolete("Use `factory.createNamespaceImport` or the factory supplied by your transformation context instead.")>]
        abstract createNamespaceImport: Identifier -> NamespaceImport
        [<Obsolete("Use `factory.updateNamespaceImport` or the factory supplied by your transformation context instead.")>]
        abstract updateNamespaceImport: NamespaceImport -> Identifier -> NamespaceImport
        [<Obsolete("Use `factory.createNamedImports` or the factory supplied by your transformation context instead.")>]
        abstract createNamedImports: ImportSpecifier[] -> NamedImports
        [<Obsolete("Use `factory.updateNamedImports` or the factory supplied by your transformation context instead.")>]
        abstract updateNamedImports: NamedImports -> ImportSpecifier[] -> NamedImports
        [<Obsolete("Use `factory.createImportSpecifier` or the factory supplied by your transformation context instead.")>]
        abstract createImportSpecifier: bool -> Identifier option -> Identifier -> ImportSpecifier
        [<Obsolete("Use `factory.updateImportSpecifier` or the factory supplied by your transformation context instead.")>]
        abstract updateImportSpecifier: ImportSpecifier -> bool -> Identifier option -> Identifier -> ImportSpecifier
        [<Obsolete("Use `factory.createExportAssignment` or the factory supplied by your transformation context instead.")>]
        abstract createExportAssignment: Decorator[] option -> Modifier[] option -> bool option -> Expression -> ExportAssignment
        [<Obsolete("Use `factory.updateExportAssignment` or the factory supplied by your transformation context instead.")>]
        abstract updateExportAssignment: ExportAssignment -> Decorator[] option -> Modifier[] option -> Expression -> ExportAssignment
        [<Obsolete("Use `factory.createNamedExports` or the factory supplied by your transformation context instead.")>]
        abstract createNamedExports: ExportSpecifier[] -> NamedExports
        [<Obsolete("Use `factory.updateNamedExports` or the factory supplied by your transformation context instead.")>]
        abstract updateNamedExports: NamedExports -> ExportSpecifier[] -> NamedExports
        [<Obsolete("Use `factory.createExportSpecifier` or the factory supplied by your transformation context instead.")>]
        abstract createExportSpecifier: bool -> U2<string, Identifier> option -> U2<string, Identifier> -> ExportSpecifier
        [<Obsolete("Use `factory.updateExportSpecifier` or the factory supplied by your transformation context instead.")>]
        abstract updateExportSpecifier: ExportSpecifier -> bool -> Identifier option -> Identifier -> ExportSpecifier
        [<Obsolete("Use `factory.createExternalModuleReference` or the factory supplied by your transformation context instead.")>]
        abstract createExternalModuleReference: Expression -> ExternalModuleReference
        [<Obsolete("Use `factory.updateExternalModuleReference` or the factory supplied by your transformation context instead.")>]
        abstract updateExternalModuleReference: ExternalModuleReference -> Expression -> ExternalModuleReference
        [<Obsolete("Use `factory.createJSDocTypeExpression` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocTypeExpression: TypeNode -> JSDocTypeExpression
        [<Obsolete("Use `factory.createJSDocTypeTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocTypeTag: Identifier option -> JSDocTypeExpression -> (U2<string, JSDocComment[]>) option -> JSDocTypeTag
        [<Obsolete("Use `factory.createJSDocReturnTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocReturnTag: Identifier option -> (JSDocTypeExpression) option -> (U2<string, JSDocComment[]>) option -> JSDocReturnTag
        [<Obsolete("Use `factory.createJSDocThisTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocThisTag: Identifier option -> JSDocTypeExpression -> (U2<string, JSDocComment[]>) option -> JSDocThisTag
        [<Obsolete("Use `factory.createJSDocComment` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocComment: (U2<string, JSDocComment[]>) option -> (JSDocTag[]) option -> JSDoc
        [<Obsolete("Use `factory.createJSDocParameterTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocParameterTag: Identifier option -> EntityName -> bool -> (JSDocTypeExpression) option -> (bool) option -> (U2<string, JSDocComment[]>) option -> JSDocParameterTag
        [<Obsolete("Use `factory.createJSDocClassTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocClassTag: Identifier option -> (U2<string, JSDocComment[]>) option -> JSDocClassTag
        [<Obsolete("Use `factory.createJSDocAugmentsTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocAugmentsTag: Identifier option -> obj -> (U2<string, JSDocComment[]>) option -> JSDocAugmentsTag
        [<Obsolete("Use `factory.createJSDocEnumTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocEnumTag: Identifier option -> JSDocTypeExpression -> (U2<string, JSDocComment[]>) option -> JSDocEnumTag
        [<Obsolete("Use `factory.createJSDocTemplateTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocTemplateTag: Identifier option -> JSDocTypeExpression option -> TypeParameterDeclaration[] -> (U2<string, JSDocComment[]>) option -> JSDocTemplateTag
        [<Obsolete("Use `factory.createJSDocTypedefTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocTypedefTag: Identifier option -> (U2<JSDocTypeLiteral, JSDocTypeExpression>) option -> (U2<Identifier, JSDocNamespaceDeclaration>) option -> (U2<string, JSDocComment[]>) option -> JSDocTypedefTag
        [<Obsolete("Use `factory.createJSDocCallbackTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocCallbackTag: Identifier option -> JSDocSignature -> (U2<Identifier, JSDocNamespaceDeclaration>) option -> (U2<string, JSDocComment[]>) option -> JSDocCallbackTag
        [<Obsolete("Use `factory.createJSDocSignature` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocSignature: JSDocTemplateTag[] option -> JSDocParameterTag[] -> (JSDocReturnTag) option -> JSDocSignature
        [<Obsolete("Use `factory.createJSDocPropertyTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocPropertyTag: Identifier option -> EntityName -> bool -> (JSDocTypeExpression) option -> (bool) option -> (U2<string, JSDocComment[]>) option -> JSDocPropertyTag
        [<Obsolete("Use `factory.createJSDocTypeLiteral` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocTypeLiteral: (JSDocPropertyLikeTag[]) option -> (bool) option -> JSDocTypeLiteral
        [<Obsolete("Use `factory.createJSDocImplementsTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocImplementsTag: Identifier option -> obj -> (U2<string, JSDocComment[]>) option -> JSDocImplementsTag
        [<Obsolete("Use `factory.createJSDocAuthorTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocAuthorTag: Identifier option -> (U2<string, JSDocComment[]>) option -> JSDocAuthorTag
        [<Obsolete("Use `factory.createJSDocPublicTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocPublicTag: Identifier option -> (U2<string, JSDocComment[]>) option -> JSDocPublicTag
        [<Obsolete("Use `factory.createJSDocPrivateTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocPrivateTag: Identifier option -> (U2<string, JSDocComment[]>) option -> JSDocPrivateTag
        [<Obsolete("Use `factory.createJSDocProtectedTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocProtectedTag: Identifier option -> (U2<string, JSDocComment[]>) option -> JSDocProtectedTag
        [<Obsolete("Use `factory.createJSDocReadonlyTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocReadonlyTag: Identifier option -> (U2<string, JSDocComment[]>) option -> JSDocReadonlyTag
        [<Obsolete("Use `factory.createJSDocUnknownTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocTag: Identifier -> (U2<string, JSDocComment[]>) option -> JSDocUnknownTag
        [<Obsolete("Use `factory.createJsxElement` or the factory supplied by your transformation context instead.")>]
        abstract createJsxElement: JsxOpeningElement -> JsxChild[] -> JsxClosingElement -> JsxElement
        [<Obsolete("Use `factory.updateJsxElement` or the factory supplied by your transformation context instead.")>]
        abstract updateJsxElement: JsxElement -> JsxOpeningElement -> JsxChild[] -> JsxClosingElement -> JsxElement
        [<Obsolete("Use `factory.createJsxSelfClosingElement` or the factory supplied by your transformation context instead.")>]
        abstract createJsxSelfClosingElement: JsxTagNameExpression -> TypeNode[] option -> JsxAttributes -> JsxSelfClosingElement
        [<Obsolete("Use `factory.updateJsxSelfClosingElement` or the factory supplied by your transformation context instead.")>]
        abstract updateJsxSelfClosingElement: JsxSelfClosingElement -> JsxTagNameExpression -> TypeNode[] option -> JsxAttributes -> JsxSelfClosingElement
        [<Obsolete("Use `factory.createJsxOpeningElement` or the factory supplied by your transformation context instead.")>]
        abstract createJsxOpeningElement: JsxTagNameExpression -> TypeNode[] option -> JsxAttributes -> JsxOpeningElement
        [<Obsolete("Use `factory.updateJsxOpeningElement` or the factory supplied by your transformation context instead.")>]
        abstract updateJsxOpeningElement: JsxOpeningElement -> JsxTagNameExpression -> TypeNode[] option -> JsxAttributes -> JsxOpeningElement
        [<Obsolete("Use `factory.createJsxClosingElement` or the factory supplied by your transformation context instead.")>]
        abstract createJsxClosingElement: JsxTagNameExpression -> JsxClosingElement
        [<Obsolete("Use `factory.updateJsxClosingElement` or the factory supplied by your transformation context instead.")>]
        abstract updateJsxClosingElement: JsxClosingElement -> JsxTagNameExpression -> JsxClosingElement
        [<Obsolete("Use `factory.createJsxFragment` or the factory supplied by your transformation context instead.")>]
        abstract createJsxFragment: JsxOpeningFragment -> JsxChild[] -> JsxClosingFragment -> JsxFragment
        [<Obsolete("Use `factory.createJsxText` or the factory supplied by your transformation context instead.")>]
        abstract createJsxText: string -> (bool) option -> JsxText
        [<Obsolete("Use `factory.updateJsxText` or the factory supplied by your transformation context instead.")>]
        abstract updateJsxText: JsxText -> string -> (bool) option -> JsxText
        [<Obsolete("Use `factory.createJsxOpeningFragment` or the factory supplied by your transformation context instead.")>]
        abstract createJsxOpeningFragment: unit -> JsxOpeningFragment
        [<Obsolete("Use `factory.createJsxJsxClosingFragment` or the factory supplied by your transformation context instead.")>]
        abstract createJsxJsxClosingFragment: unit -> JsxClosingFragment
        [<Obsolete("Use `factory.updateJsxFragment` or the factory supplied by your transformation context instead.")>]
        abstract updateJsxFragment: JsxFragment -> JsxOpeningFragment -> JsxChild[] -> JsxClosingFragment -> JsxFragment
        [<Obsolete("Use `factory.createJsxAttribute` or the factory supplied by your transformation context instead.")>]
        abstract createJsxAttribute: Identifier -> U2<StringLiteral, JsxExpression> option -> JsxAttribute
        [<Obsolete("Use `factory.updateJsxAttribute` or the factory supplied by your transformation context instead.")>]
        abstract updateJsxAttribute: JsxAttribute -> Identifier -> U2<StringLiteral, JsxExpression> option -> JsxAttribute
        [<Obsolete("Use `factory.createJsxAttributes` or the factory supplied by your transformation context instead.")>]
        abstract createJsxAttributes: JsxAttributeLike[] -> JsxAttributes
        [<Obsolete("Use `factory.updateJsxAttributes` or the factory supplied by your transformation context instead.")>]
        abstract updateJsxAttributes: JsxAttributes -> JsxAttributeLike[] -> JsxAttributes
        [<Obsolete("Use `factory.createJsxSpreadAttribute` or the factory supplied by your transformation context instead.")>]
        abstract createJsxSpreadAttribute: Expression -> JsxSpreadAttribute
        [<Obsolete("Use `factory.updateJsxSpreadAttribute` or the factory supplied by your transformation context instead.")>]
        abstract updateJsxSpreadAttribute: JsxSpreadAttribute -> Expression -> JsxSpreadAttribute
        [<Obsolete("Use `factory.createJsxExpression` or the factory supplied by your transformation context instead.")>]
        abstract createJsxExpression: DotDotDotToken option -> Expression option -> JsxExpression
        [<Obsolete("Use `factory.updateJsxExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateJsxExpression: JsxExpression -> Expression option -> JsxExpression
        [<Obsolete("Use `factory.createCaseClause` or the factory supplied by your transformation context instead.")>]
        abstract createCaseClause: Expression -> Statement[] -> CaseClause
        [<Obsolete("Use `factory.updateCaseClause` or the factory supplied by your transformation context instead.")>]
        abstract updateCaseClause: CaseClause -> Expression -> Statement[] -> CaseClause
        [<Obsolete("Use `factory.createDefaultClause` or the factory supplied by your transformation context instead.")>]
        abstract createDefaultClause: Statement[] -> DefaultClause
        [<Obsolete("Use `factory.updateDefaultClause` or the factory supplied by your transformation context instead.")>]
        abstract updateDefaultClause: DefaultClause -> Statement[] -> DefaultClause
        [<Obsolete("Use `factory.createHeritageClause` or the factory supplied by your transformation context instead.")>]
        abstract createHeritageClause: SyntaxKind -> ExpressionWithTypeArguments[] -> HeritageClause
        [<Obsolete("Use `factory.updateHeritageClause` or the factory supplied by your transformation context instead.")>]
        abstract updateHeritageClause: HeritageClause -> ExpressionWithTypeArguments[] -> HeritageClause
        [<Obsolete("Use `factory.createCatchClause` or the factory supplied by your transformation context instead.")>]
        abstract createCatchClause: U3<string, VariableDeclaration, BindingName> option -> Block -> CatchClause
        [<Obsolete("Use `factory.updateCatchClause` or the factory supplied by your transformation context instead.")>]
        abstract updateCatchClause: CatchClause -> VariableDeclaration option -> Block -> CatchClause
        [<Obsolete("Use `factory.createPropertyAssignment` or the factory supplied by your transformation context instead.")>]
        abstract createPropertyAssignment: U2<string, PropertyName> -> Expression -> PropertyAssignment
        [<Obsolete("Use `factory.updatePropertyAssignment` or the factory supplied by your transformation context instead.")>]
        abstract updatePropertyAssignment: PropertyAssignment -> PropertyName -> Expression -> PropertyAssignment
        [<Obsolete("Use `factory.createShorthandPropertyAssignment` or the factory supplied by your transformation context instead.")>]
        abstract createShorthandPropertyAssignment: U2<string, Identifier> -> (Expression) option -> ShorthandPropertyAssignment
        [<Obsolete("Use `factory.updateShorthandPropertyAssignment` or the factory supplied by your transformation context instead.")>]
        abstract updateShorthandPropertyAssignment: ShorthandPropertyAssignment -> Identifier -> Expression option -> ShorthandPropertyAssignment
        [<Obsolete("Use `factory.createSpreadAssignment` or the factory supplied by your transformation context instead.")>]
        abstract createSpreadAssignment: Expression -> SpreadAssignment
        [<Obsolete("Use `factory.updateSpreadAssignment` or the factory supplied by your transformation context instead.")>]
        abstract updateSpreadAssignment: SpreadAssignment -> Expression -> SpreadAssignment
        [<Obsolete("Use `factory.createEnumMember` or the factory supplied by your transformation context instead.")>]
        abstract createEnumMember: U2<string, PropertyName> -> (Expression) option -> EnumMember
        [<Obsolete("Use `factory.updateEnumMember` or the factory supplied by your transformation context instead.")>]
        abstract updateEnumMember: EnumMember -> PropertyName -> Expression option -> EnumMember
        [<Obsolete("Use `factory.updateSourceFile` or the factory supplied by your transformation context instead.")>]
        abstract updateSourceFileNode: SourceFile -> Statement[] -> (bool) option -> (FileReference[]) option -> (FileReference[]) option -> (bool) option -> (FileReference[]) option -> SourceFile
        [<Obsolete("Use `factory.createNotEmittedStatement` or the factory supplied by your transformation context instead.")>]
        abstract createNotEmittedStatement: Node -> NotEmittedStatement
        [<Obsolete("Use `factory.createPartiallyEmittedExpression` or the factory supplied by your transformation context instead.")>]
        abstract createPartiallyEmittedExpression: Expression -> (Node) option -> PartiallyEmittedExpression
        [<Obsolete("Use `factory.updatePartiallyEmittedExpression` or the factory supplied by your transformation context instead.")>]
        abstract updatePartiallyEmittedExpression: PartiallyEmittedExpression -> Expression -> PartiallyEmittedExpression
        [<Obsolete("Use `factory.createCommaListExpression` or the factory supplied by your transformation context instead.")>]
        abstract createCommaList: Expression[] -> CommaListExpression
        [<Obsolete("Use `factory.updateCommaListExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateCommaList: CommaListExpression -> Expression[] -> CommaListExpression
        [<Obsolete("Use `factory.createBundle` or the factory supplied by your transformation context instead.")>]
        abstract createBundle: SourceFile[] -> (U2<UnparsedSource, InputFiles>[]) option -> Bundle
        [<Obsolete("Use `factory.updateBundle` or the factory supplied by your transformation context instead.")>]
        abstract updateBundle: Bundle -> SourceFile[] -> (U2<UnparsedSource, InputFiles>[]) option -> Bundle
        [<Obsolete("Use `factory.createImmediatelyInvokedFunctionExpression` or the factory supplied by your transformation context instead.")>]
        abstract createImmediatelyInvokedFunctionExpression: {| Invoke: Statement[] -> CallExpression; Invoke: Statement[] -> ParameterDeclaration -> Expression -> CallExpression |}
        [<Obsolete("Use `factory.createImmediatelyInvokedArrowFunction` or the factory supplied by your transformation context instead.")>]
        abstract createImmediatelyInvokedArrowFunction: {| Invoke: Statement[] -> CallExpression; Invoke: Statement[] -> ParameterDeclaration -> Expression -> CallExpression |}
        [<Obsolete("Use `factory.createVoidZero` or the factory supplied by your transformation context instead.")>]
        abstract createVoidZero: unit -> VoidExpression
        [<Obsolete("Use `factory.createExportDefault` or the factory supplied by your transformation context instead.")>]
        abstract createExportDefault: Expression -> ExportAssignment
        [<Obsolete("Use `factory.createExternalModuleExport` or the factory supplied by your transformation context instead.")>]
        abstract createExternalModuleExport: Identifier -> ExportDeclaration
        [<Obsolete("Use `factory.createNamespaceExport` or the factory supplied by your transformation context instead.")>]
        abstract createNamespaceExport: Identifier -> NamespaceExport
        [<Obsolete("Use `factory.updateNamespaceExport` or the factory supplied by your transformation context instead.")>]
        abstract updateNamespaceExport: NamespaceExport -> Identifier -> NamespaceExport
        [<Obsolete("Use `factory.createToken` or the factory supplied by your transformation context instead.")>]
        abstract createToken: 'TKind -> Token<'TKind>
        [<Obsolete("Use `factory.createIdentifier` or the factory supplied by your transformation context instead.")>]
        abstract createIdentifier: string -> Identifier
        [<Obsolete("Use `factory.createTempVariable` or the factory supplied by your transformation context instead.")>]
        abstract createTempVariable: (Identifier -> unit) option -> Identifier
        [<Obsolete("Use `factory.getGeneratedNameForNode` or the factory supplied by your transformation context instead.")>]
        abstract getGeneratedNameForNode: Node option -> Identifier
        [<Obsolete("Use `factory.createUniqueName(text, GeneratedIdentifierFlags.Optimistic)` or the factory supplied by your transformation context instead.")>]
        abstract createOptimisticUniqueName: string -> Identifier
        [<Obsolete("Use `factory.createUniqueName(text, GeneratedIdentifierFlags.Optimistic | GeneratedIdentifierFlags.FileLevel)` or the factory supplied by your transformation context instead.")>]
        abstract createFileLevelUniqueName: string -> Identifier
        [<Obsolete("Use `factory.createIndexSignature` or the factory supplied by your transformation context instead.")>]
        abstract createIndexSignature: Decorator[] option -> Modifier[] option -> ParameterDeclaration[] -> TypeNode -> IndexSignatureDeclaration
        [<Obsolete("Use `factory.createTypePredicateNode` or the factory supplied by your transformation context instead.")>]
        abstract createTypePredicateNode: U3<Identifier, ThisTypeNode, string> -> TypeNode -> TypePredicateNode
        [<Obsolete("Use `factory.updateTypePredicateNode` or the factory supplied by your transformation context instead.")>]
        abstract updateTypePredicateNode: TypePredicateNode -> U2<Identifier, ThisTypeNode> -> TypeNode -> TypePredicateNode
        [<Obsolete("Use `factory.createStringLiteral`, `factory.createStringLiteralFromNode`, `factory.createNumericLiteral`, `factory.createBigIntLiteral`, `factory.createTrue`, `factory.createFalse`, or the factory supplied by your transformation context instead.")>]
        abstract createLiteral: {| Invoke: U5<string, StringLiteral, NoSubstitutionTemplateLiteral, NumericLiteral, Identifier> -> StringLiteral; Invoke: U2<float, PseudoBigInt> -> NumericLiteral; Invoke: bool -> BooleanLiteral; Invoke: U4<string, float, PseudoBigInt, bool> -> PrimaryExpression |}
        [<Obsolete("Use `factory.createMethodSignature` or the factory supplied by your transformation context instead.")>]
        abstract createMethodSignature: TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> U2<string, PropertyName> -> QuestionToken option -> MethodSignature
        [<Obsolete("Use `factory.updateMethodSignature` or the factory supplied by your transformation context instead.")>]
        abstract updateMethodSignature: MethodSignature -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> PropertyName -> QuestionToken option -> MethodSignature
        [<Obsolete("Use `factory.createTypeOperatorNode` or the factory supplied by your transformation context instead.")>]
        abstract createTypeOperatorNode: {| Invoke: TypeNode -> TypeOperatorNode; Invoke: SyntaxKind -> TypeNode -> TypeOperatorNode |}
        [<Obsolete("Use `factory.createTaggedTemplate` or the factory supplied by your transformation context instead.")>]
        abstract createTaggedTemplate: {| Invoke: Expression -> TemplateLiteral -> TaggedTemplateExpression; Invoke: Expression -> TypeNode[] option -> TemplateLiteral -> TaggedTemplateExpression |}
        [<Obsolete("Use `factory.updateTaggedTemplate` or the factory supplied by your transformation context instead.")>]
        abstract updateTaggedTemplate: {| Invoke: TaggedTemplateExpression -> Expression -> TemplateLiteral -> TaggedTemplateExpression; Invoke: TaggedTemplateExpression -> Expression -> TypeNode[] option -> TemplateLiteral -> TaggedTemplateExpression |}
        [<Obsolete("Use `factory.updateBinary` or the factory supplied by your transformation context instead.")>]
        abstract updateBinary: BinaryExpression -> Expression -> Expression -> (U2<BinaryOperator, BinaryOperatorToken>) option -> BinaryExpression
        [<Obsolete("Use `factory.createConditional` or the factory supplied by your transformation context instead.")>]
        abstract createConditional: {| Invoke: Expression -> Expression -> Expression -> ConditionalExpression; Invoke: Expression -> QuestionToken -> Expression -> ColonToken -> Expression -> ConditionalExpression |}
        [<Obsolete("Use `factory.createYield` or the factory supplied by your transformation context instead.")>]
        abstract createYield: {| Invoke: Expression option -> YieldExpression; Invoke: AsteriskToken option -> Expression -> YieldExpression |}
        [<Obsolete("Use `factory.createClassExpression` or the factory supplied by your transformation context instead.")>]
        abstract createClassExpression: Modifier[] option -> U2<string, Identifier> option -> TypeParameterDeclaration[] option -> HeritageClause[] option -> ClassElement[] -> ClassExpression
        [<Obsolete("Use `factory.updateClassExpression` or the factory supplied by your transformation context instead.")>]
        abstract updateClassExpression: ClassExpression -> Modifier[] option -> Identifier option -> TypeParameterDeclaration[] option -> HeritageClause[] option -> ClassElement[] -> ClassExpression
        [<Obsolete("Use `factory.createPropertySignature` or the factory supplied by your transformation context instead.")>]
        abstract createPropertySignature: Modifier[] option -> U2<PropertyName, string> -> QuestionToken option -> TypeNode option -> (Expression) option -> PropertySignature
        [<Obsolete("Use `factory.updatePropertySignature` or the factory supplied by your transformation context instead.")>]
        abstract updatePropertySignature: PropertySignature -> Modifier[] option -> PropertyName -> QuestionToken option -> TypeNode option -> Expression option -> PropertySignature
        [<Obsolete("Use `factory.createExpressionWithTypeArguments` or the factory supplied by your transformation context instead.")>]
        abstract createExpressionWithTypeArguments: TypeNode[] option -> Expression -> ExpressionWithTypeArguments
        [<Obsolete("Use `factory.updateExpressionWithTypeArguments` or the factory supplied by your transformation context instead.")>]
        abstract updateExpressionWithTypeArguments: ExpressionWithTypeArguments -> TypeNode[] option -> Expression -> ExpressionWithTypeArguments
        [<Obsolete("Use `factory.createArrowFunction` or the factory supplied by your transformation context instead.")>]
        abstract createArrowFunction: {| Invoke: Modifier[] option -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> EqualsGreaterThanToken option -> ConciseBody -> ArrowFunction; Invoke: Modifier[] option -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> ConciseBody -> ArrowFunction |}
        [<Obsolete("Use `factory.updateArrowFunction` or the factory supplied by your transformation context instead.")>]
        abstract updateArrowFunction: {| Invoke: ArrowFunction -> Modifier[] option -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> EqualsGreaterThanToken -> ConciseBody -> ArrowFunction; Invoke: ArrowFunction -> Modifier[] option -> TypeParameterDeclaration[] option -> ParameterDeclaration[] -> TypeNode option -> ConciseBody -> ArrowFunction |}
        [<Obsolete("Use `factory.createVariableDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createVariableDeclaration: {| Invoke: U2<string, BindingName> -> TypeNode option -> Expression option -> VariableDeclaration; Invoke: U2<string, BindingName> -> ExclamationToken option -> TypeNode option -> Expression option -> VariableDeclaration |}
        [<Obsolete("Use `factory.updateVariableDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateVariableDeclaration: {| Invoke: VariableDeclaration -> BindingName -> TypeNode option -> Expression option -> VariableDeclaration; Invoke: VariableDeclaration -> BindingName -> ExclamationToken option -> TypeNode option -> Expression option -> VariableDeclaration |}
        [<Obsolete("Use `factory.createImportClause` or the factory supplied by your transformation context instead.")>]
        abstract createImportClause: Identifier option -> NamedImportBindings option -> (obj) option -> ImportClause
        [<Obsolete("Use `factory.updateImportClause` or the factory supplied by your transformation context instead.")>]
        abstract updateImportClause: ImportClause -> Identifier option -> NamedImportBindings option -> bool -> ImportClause
        [<Obsolete("Use `factory.createExportDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract createExportDeclaration: Decorator[] option -> Modifier[] option -> NamedExportBindings option -> (Expression) option -> (obj) option -> ExportDeclaration
        [<Obsolete("Use `factory.updateExportDeclaration` or the factory supplied by your transformation context instead.")>]
        abstract updateExportDeclaration: ExportDeclaration -> Decorator[] option -> Modifier[] option -> NamedExportBindings option -> Expression option -> bool -> ExportDeclaration
        [<Obsolete("Use `factory.createJSDocParameterTag` or the factory supplied by your transformation context instead.")>]
        abstract createJSDocParamTag: EntityName -> bool -> (JSDocTypeExpression) option -> (string) option -> JSDocParameterTag
        [<Obsolete("Use `factory.createComma` or the factory supplied by your transformation context instead.")>]
        abstract createComma: Expression -> Expression -> Expression
        [<Obsolete("Use `factory.createLessThan` or the factory supplied by your transformation context instead.")>]
        abstract createLessThan: Expression -> Expression -> Expression
        [<Obsolete("Use `factory.createAssignment` or the factory supplied by your transformation context instead.")>]
        abstract createAssignment: Expression -> Expression -> BinaryExpression
        [<Obsolete("Use `factory.createStrictEquality` or the factory supplied by your transformation context instead.")>]
        abstract createStrictEquality: Expression -> Expression -> BinaryExpression
        [<Obsolete("Use `factory.createStrictInequality` or the factory supplied by your transformation context instead.")>]
        abstract createStrictInequality: Expression -> Expression -> BinaryExpression
        [<Obsolete("Use `factory.createAdd` or the factory supplied by your transformation context instead.")>]
        abstract createAdd: Expression -> Expression -> BinaryExpression
        [<Obsolete("Use `factory.createSubtract` or the factory supplied by your transformation context instead.")>]
        abstract createSubtract: Expression -> Expression -> BinaryExpression
        [<Obsolete("Use `factory.createLogicalAnd` or the factory supplied by your transformation context instead.")>]
        abstract createLogicalAnd: Expression -> Expression -> BinaryExpression
        [<Obsolete("Use `factory.createLogicalOr` or the factory supplied by your transformation context instead.")>]
        abstract createLogicalOr: Expression -> Expression -> BinaryExpression
        [<Obsolete("Use `factory.createPostfixIncrement` or the factory supplied by your transformation context instead.")>]
        abstract createPostfixIncrement: Expression -> PostfixUnaryExpression
        [<Obsolete("Use `factory.createLogicalNot` or the factory supplied by your transformation context instead.")>]
        abstract createLogicalNot: Expression -> PrefixUnaryExpression
        [<Obsolete("Use an appropriate `factory` method instead.")>]
        abstract createNode: SyntaxKind -> (obj) option -> (obj) option -> Node
        /// <summary>
        /// Creates a shallow, memberwise clone of a node ~for mutation~ with its <c>pos</c>, <c>end</c>, and <c>parent</c> set.
        ///
        /// NOTE: It is unsafe to change any properties of a <c>Node</c> that relate to its AST children, as those changes won't be
        /// captured with respect to transformations.
        /// </summary>
        [<Obsolete("Use an appropriate `factory.update...` method instead, use `setCommentRange` or `setSourceMapRange`, and avoid setting `parent`.")>]
        abstract getMutableClone: 'T -> 'T
        [<Obsolete("Use `isTypeAssertionExpression` instead.")>]
        abstract isTypeAssertion: Node -> bool
        [<Obsolete("Use `isMemberName` instead.")>]
        abstract isIdentifierOrPrivateIdentifier: Node -> bool

    /// <summary>
    /// Type of objects whose values are all of the same type.
    /// The <c>in</c> and <c>for-in</c> operators can *not* be safely used,
    /// since <c>Object.prototype</c> may be modified by outside code.
    /// </summary>
    type [<AllowNullLiteral>] MapLike<'T> =
        [<EmitIndexer>] abstract Item: index: string -> 'T with get, set

    type [<AllowNullLiteral>] SortedReadonlyArray<'T> =
        inherit ReadonlyArray<'T>
        abstract `` __sortedArrayBrand``: obj option with get, set

    type [<AllowNullLiteral>] SortedArray<'T> =
        inherit Array<'T>
        abstract `` __sortedArrayBrand``: obj option with get, set

    /// Common read methods for ES6 Map/Set.
    type [<AllowNullLiteral>] ReadonlyCollection<'K> =
        abstract size: float
        abstract has: key: 'K -> bool
        abstract keys: unit -> Iterator<'K>

    /// Common write methods for ES6 Map/Set.
    type [<AllowNullLiteral>] Collection<'K> =
        inherit ReadonlyCollection<'K>
        abstract delete: key: 'K -> bool
        abstract clear: unit -> unit

    /// ES6 Map interface, only read methods included.
    type [<AllowNullLiteral>] ReadonlyESMap<'K, 'V> =
        inherit ReadonlyCollection<'K>
        abstract get: key: 'K -> 'V option
        abstract values: unit -> Iterator<'V>
        abstract entries: unit -> Iterator<'K * 'V>
        abstract forEach: action: ('V -> 'K -> unit) -> unit

    /// ES6 Map interface, only read methods included.
    type [<AllowNullLiteral>] ReadonlyMap<'T> =
        inherit ReadonlyESMap<string, 'T>

    /// ES6 Map interface.
    type [<AllowNullLiteral>] ESMap<'K, 'V> =
        inherit ReadonlyESMap<'K, 'V>
        inherit Collection<'K>
        abstract set: key: 'K * value: 'V -> ESMap<'K, 'V>

    /// ES6 Map interface.
    type [<AllowNullLiteral>] Map<'T> =
        inherit ESMap<string, 'T>

    /// ES6 Set interface, only read methods included.
    type [<AllowNullLiteral>] ReadonlySet<'T> =
        inherit ReadonlyCollection<'T>
        abstract has: value: 'T -> bool
        abstract values: unit -> Iterator<'T>
        abstract entries: unit -> Iterator<'T * 'T>
        abstract forEach: action: ('T -> 'T -> unit) -> unit

    /// ES6 Set interface.
    type [<AllowNullLiteral>] Set<'T> =
        inherit ReadonlySet<'T>
        inherit Collection<'T>
        abstract add: value: 'T -> Set<'T>
        abstract delete: value: 'T -> bool

    /// ES6 Iterator type.
    type [<AllowNullLiteral>] Iterator<'T> =
        abstract next: unit -> U2<{| value: 'T; ``done``: bool option |}, {| value: unit; ``done``: bool |}>

    /// Array that is only intended to be pushed to, never read.
    type [<AllowNullLiteral>] Push<'T> =
        abstract push: [<ParamArray>] values: 'T[] -> unit

    type [<AllowNullLiteral>] Path =
        interface end

    type [<AllowNullLiteral>] TextRange =
        abstract pos: float with get, set
        abstract ``end``: float with get, set

    type [<AllowNullLiteral>] ReadonlyTextRange =
        abstract pos: float
        abstract ``end``: float

    type [<RequireQualifiedAccess>] SyntaxKind =
        | Unknown = 0
        | EndOfFileToken = 1
        | SingleLineCommentTrivia = 2
        | MultiLineCommentTrivia = 3
        | NewLineTrivia = 4
        | WhitespaceTrivia = 5
        | ShebangTrivia = 6
        | ConflictMarkerTrivia = 7
        | NumericLiteral = 8
        | BigIntLiteral = 9
        | StringLiteral = 10
        | JsxText = 11
        | JsxTextAllWhiteSpaces = 12
        | RegularExpressionLiteral = 13
        | NoSubstitutionTemplateLiteral = 14
        | TemplateHead = 15
        | TemplateMiddle = 16
        | TemplateTail = 17
        | OpenBraceToken = 18
        | CloseBraceToken = 19
        | OpenParenToken = 20
        | CloseParenToken = 21
        | OpenBracketToken = 22
        | CloseBracketToken = 23
        | DotToken = 24
        | DotDotDotToken = 25
        | SemicolonToken = 26
        | CommaToken = 27
        | QuestionDotToken = 28
        | LessThanToken = 29
        | LessThanSlashToken = 30
        | GreaterThanToken = 31
        | LessThanEqualsToken = 32
        | GreaterThanEqualsToken = 33
        | EqualsEqualsToken = 34
        | ExclamationEqualsToken = 35
        | EqualsEqualsEqualsToken = 36
        | ExclamationEqualsEqualsToken = 37
        | EqualsGreaterThanToken = 38
        | PlusToken = 39
        | MinusToken = 40
        | AsteriskToken = 41
        | AsteriskAsteriskToken = 42
        | SlashToken = 43
        | PercentToken = 44
        | PlusPlusToken = 45
        | MinusMinusToken = 46
        | LessThanLessThanToken = 47
        | GreaterThanGreaterThanToken = 48
        | GreaterThanGreaterThanGreaterThanToken = 49
        | AmpersandToken = 50
        | BarToken = 51
        | CaretToken = 52
        | ExclamationToken = 53
        | TildeToken = 54
        | AmpersandAmpersandToken = 55
        | BarBarToken = 56
        | QuestionToken = 57
        | ColonToken = 58
        | AtToken = 59
        | QuestionQuestionToken = 60
        /// Only the JSDoc scanner produces BacktickToken. The normal scanner produces NoSubstitutionTemplateLiteral and related kinds.
        | BacktickToken = 61
        /// Only the JSDoc scanner produces HashToken. The normal scanner produces PrivateIdentifier.
        | HashToken = 62
        | EqualsToken = 63
        | PlusEqualsToken = 64
        | MinusEqualsToken = 65
        | AsteriskEqualsToken = 66
        | AsteriskAsteriskEqualsToken = 67
        | SlashEqualsToken = 68
        | PercentEqualsToken = 69
        | LessThanLessThanEqualsToken = 70
        | GreaterThanGreaterThanEqualsToken = 71
        | GreaterThanGreaterThanGreaterThanEqualsToken = 72
        | AmpersandEqualsToken = 73
        | BarEqualsToken = 74
        | BarBarEqualsToken = 75
        | AmpersandAmpersandEqualsToken = 76
        | QuestionQuestionEqualsToken = 77
        | CaretEqualsToken = 78
        | Identifier = 79
        | PrivateIdentifier = 80
        | BreakKeyword = 81
        | CaseKeyword = 82
        | CatchKeyword = 83
        | ClassKeyword = 84
        | ConstKeyword = 85
        | ContinueKeyword = 86
        | DebuggerKeyword = 87
        | DefaultKeyword = 88
        | DeleteKeyword = 89
        | DoKeyword = 90
        | ElseKeyword = 91
        | EnumKeyword = 92
        | ExportKeyword = 93
        | ExtendsKeyword = 94
        | FalseKeyword = 95
        | FinallyKeyword = 96
        | ForKeyword = 97
        | FunctionKeyword = 98
        | IfKeyword = 99
        | ImportKeyword = 100
        | InKeyword = 101
        | InstanceOfKeyword = 102
        | NewKeyword = 103
        | NullKeyword = 104
        | ReturnKeyword = 105
        | SuperKeyword = 106
        | SwitchKeyword = 107
        | ThisKeyword = 108
        | ThrowKeyword = 109
        | TrueKeyword = 110
        | TryKeyword = 111
        | TypeOfKeyword = 112
        | VarKeyword = 113
        | VoidKeyword = 114
        | WhileKeyword = 115
        | WithKeyword = 116
        | ImplementsKeyword = 117
        | InterfaceKeyword = 118
        | LetKeyword = 119
        | PackageKeyword = 120
        | PrivateKeyword = 121
        | ProtectedKeyword = 122
        | PublicKeyword = 123
        | StaticKeyword = 124
        | YieldKeyword = 125
        | AbstractKeyword = 126
        | AsKeyword = 127
        | AssertsKeyword = 128
        | AssertKeyword = 129
        | AnyKeyword = 130
        | AsyncKeyword = 131
        | AwaitKeyword = 132
        | BooleanKeyword = 133
        | ConstructorKeyword = 134
        | DeclareKeyword = 135
        | GetKeyword = 136
        | InferKeyword = 137
        | IntrinsicKeyword = 138
        | IsKeyword = 139
        | KeyOfKeyword = 140
        | ModuleKeyword = 141
        | NamespaceKeyword = 142
        | NeverKeyword = 143
        | ReadonlyKeyword = 144
        | RequireKeyword = 145
        | NumberKeyword = 146
        | ObjectKeyword = 147
        | SetKeyword = 148
        | StringKeyword = 149
        | SymbolKeyword = 150
        | TypeKeyword = 151
        | UndefinedKeyword = 152
        | UniqueKeyword = 153
        | UnknownKeyword = 154
        | FromKeyword = 155
        | GlobalKeyword = 156
        | BigIntKeyword = 157
        | OverrideKeyword = 158
        | OfKeyword = 159
        | QualifiedName = 160
        | ComputedPropertyName = 161
        | TypeParameter = 162
        | Parameter = 163
        | Decorator = 164
        | PropertySignature = 165
        | PropertyDeclaration = 166
        | MethodSignature = 167
        | MethodDeclaration = 168
        | ClassStaticBlockDeclaration = 169
        | Constructor = 170
        | GetAccessor = 171
        | SetAccessor = 172
        | CallSignature = 173
        | ConstructSignature = 174
        | IndexSignature = 175
        | TypePredicate = 176
        | TypeReference = 177
        | FunctionType = 178
        | ConstructorType = 179
        | TypeQuery = 180
        | TypeLiteral = 181
        | ArrayType = 182
        | TupleType = 183
        | OptionalType = 184
        | RestType = 185
        | UnionType = 186
        | IntersectionType = 187
        | ConditionalType = 188
        | InferType = 189
        | ParenthesizedType = 190
        | ThisType = 191
        | TypeOperator = 192
        | IndexedAccessType = 193
        | MappedType = 194
        | LiteralType = 195
        | NamedTupleMember = 196
        | TemplateLiteralType = 197
        | TemplateLiteralTypeSpan = 198
        | ImportType = 199
        | ObjectBindingPattern = 200
        | ArrayBindingPattern = 201
        | BindingElement = 202
        | ArrayLiteralExpression = 203
        | ObjectLiteralExpression = 204
        | PropertyAccessExpression = 205
        | ElementAccessExpression = 206
        | CallExpression = 207
        | NewExpression = 208
        | TaggedTemplateExpression = 209
        | TypeAssertionExpression = 210
        | ParenthesizedExpression = 211
        | FunctionExpression = 212
        | ArrowFunction = 213
        | DeleteExpression = 214
        | TypeOfExpression = 215
        | VoidExpression = 216
        | AwaitExpression = 217
        | PrefixUnaryExpression = 218
        | PostfixUnaryExpression = 219
        | BinaryExpression = 220
        | ConditionalExpression = 221
        | TemplateExpression = 222
        | YieldExpression = 223
        | SpreadElement = 224
        | ClassExpression = 225
        | OmittedExpression = 226
        | ExpressionWithTypeArguments = 227
        | AsExpression = 228
        | NonNullExpression = 229
        | MetaProperty = 230
        | SyntheticExpression = 231
        | TemplateSpan = 232
        | SemicolonClassElement = 233
        | Block = 234
        | EmptyStatement = 235
        | VariableStatement = 236
        | ExpressionStatement = 237
        | IfStatement = 238
        | DoStatement = 239
        | WhileStatement = 240
        | ForStatement = 241
        | ForInStatement = 242
        | ForOfStatement = 243
        | ContinueStatement = 244
        | BreakStatement = 245
        | ReturnStatement = 246
        | WithStatement = 247
        | SwitchStatement = 248
        | LabeledStatement = 249
        | ThrowStatement = 250
        | TryStatement = 251
        | DebuggerStatement = 252
        | VariableDeclaration = 253
        | VariableDeclarationList = 254
        | FunctionDeclaration = 255
        | ClassDeclaration = 256
        | InterfaceDeclaration = 257
        | TypeAliasDeclaration = 258
        | EnumDeclaration = 259
        | ModuleDeclaration = 260
        | ModuleBlock = 261
        | CaseBlock = 262
        | NamespaceExportDeclaration = 263
        | ImportEqualsDeclaration = 264
        | ImportDeclaration = 265
        | ImportClause = 266
        | NamespaceImport = 267
        | NamedImports = 268
        | ImportSpecifier = 269
        | ExportAssignment = 270
        | ExportDeclaration = 271
        | NamedExports = 272
        | NamespaceExport = 273
        | ExportSpecifier = 274
        | MissingDeclaration = 275
        | ExternalModuleReference = 276
        | JsxElement = 277
        | JsxSelfClosingElement = 278
        | JsxOpeningElement = 279
        | JsxClosingElement = 280
        | JsxFragment = 281
        | JsxOpeningFragment = 282
        | JsxClosingFragment = 283
        | JsxAttribute = 284
        | JsxAttributes = 285
        | JsxSpreadAttribute = 286
        | JsxExpression = 287
        | CaseClause = 288
        | DefaultClause = 289
        | HeritageClause = 290
        | CatchClause = 291
        | AssertClause = 292
        | AssertEntry = 293
        | PropertyAssignment = 294
        | ShorthandPropertyAssignment = 295
        | SpreadAssignment = 296
        | EnumMember = 297
        | UnparsedPrologue = 298
        | UnparsedPrepend = 299
        | UnparsedText = 300
        | UnparsedInternalText = 301
        | UnparsedSyntheticReference = 302
        | SourceFile = 303
        | Bundle = 304
        | UnparsedSource = 305
        | InputFiles = 306
        | JSDocTypeExpression = 307
        | JSDocNameReference = 308
        | JSDocMemberName = 309
        | JSDocAllType = 310
        | JSDocUnknownType = 311
        | JSDocNullableType = 312
        | JSDocNonNullableType = 313
        | JSDocOptionalType = 314
        | JSDocFunctionType = 315
        | JSDocVariadicType = 316
        | JSDocNamepathType = 317
        | JSDocComment = 318
        | JSDocText = 319
        | JSDocTypeLiteral = 320
        | JSDocSignature = 321
        | JSDocLink = 322
        | JSDocLinkCode = 323
        | JSDocLinkPlain = 324
        | JSDocTag = 325
        | JSDocAugmentsTag = 326
        | JSDocImplementsTag = 327
        | JSDocAuthorTag = 328
        | JSDocDeprecatedTag = 329
        | JSDocClassTag = 330
        | JSDocPublicTag = 331
        | JSDocPrivateTag = 332
        | JSDocProtectedTag = 333
        | JSDocReadonlyTag = 334
        | JSDocOverrideTag = 335
        | JSDocCallbackTag = 336
        | JSDocEnumTag = 337
        | JSDocParameterTag = 338
        | JSDocReturnTag = 339
        | JSDocThisTag = 340
        | JSDocTypeTag = 341
        | JSDocTemplateTag = 342
        | JSDocTypedefTag = 343
        | JSDocSeeTag = 344
        | JSDocPropertyTag = 345
        | SyntaxList = 346
        | NotEmittedStatement = 347
        | PartiallyEmittedExpression = 348
        | CommaListExpression = 349
        | MergeDeclarationMarker = 350
        | EndOfDeclarationMarker = 351
        | SyntheticReferenceExpression = 352
        | Count = 353
        | FirstAssignment = 63
        | LastAssignment = 78
        | FirstCompoundAssignment = 64
        | LastCompoundAssignment = 78
        | FirstReservedWord = 81
        | LastReservedWord = 116
        | FirstKeyword = 81
        | LastKeyword = 159
        | FirstFutureReservedWord = 117
        | LastFutureReservedWord = 125
        | FirstTypeNode = 176
        | LastTypeNode = 199
        | FirstPunctuation = 18
        | LastPunctuation = 78
        | FirstToken = 0
        | LastToken = 159
        | FirstTriviaToken = 2
        | LastTriviaToken = 7
        | FirstLiteralToken = 8
        | LastLiteralToken = 14
        | FirstTemplateToken = 14
        | LastTemplateToken = 17
        | FirstBinaryOperator = 29
        | LastBinaryOperator = 78
        | FirstStatement = 236
        | LastStatement = 252
        | FirstNode = 160
        | FirstJSDocNode = 307
        | LastJSDocNode = 345
        | FirstJSDocTagNode = 325
        | LastJSDocTagNode = 345

    type TriviaSyntaxKind =
        SyntaxKind

    type LiteralSyntaxKind =
        SyntaxKind

    type PseudoLiteralSyntaxKind =
        SyntaxKind

    type PunctuationSyntaxKind =
        SyntaxKind

    type KeywordSyntaxKind =
        SyntaxKind

    type ModifierSyntaxKind =
        SyntaxKind

    type KeywordTypeSyntaxKind =
        SyntaxKind

    type TokenSyntaxKind =
        U6<SyntaxKind, TriviaSyntaxKind, LiteralSyntaxKind, PseudoLiteralSyntaxKind, PunctuationSyntaxKind, KeywordSyntaxKind>

    type JsxTokenSyntaxKind =
        SyntaxKind

    type JSDocSyntaxKind =
        U2<SyntaxKind, KeywordSyntaxKind>

    type [<RequireQualifiedAccess>] NodeFlags =
        | None = 0
        | Let = 1
        | Const = 2
        | NestedNamespace = 4
        | Synthesized = 8
        | Namespace = 16
        | OptionalChain = 32
        | ExportContext = 64
        | ContainsThis = 128
        | HasImplicitReturn = 256
        | HasExplicitReturn = 512
        | GlobalAugmentation = 1024
        | HasAsyncFunctions = 2048
        | DisallowInContext = 4096
        | YieldContext = 8192
        | DecoratorContext = 16384
        | AwaitContext = 32768
        | ThisNodeHasError = 65536
        | JavaScriptFile = 131072
        | ThisNodeOrAnySubNodesHasError = 262144
        | HasAggregatedChildData = 524288
        | JSDoc = 4194304
        | JsonFile = 33554432
        | BlockScoped = 3
        | ReachabilityCheckFlags = 768
        | ReachabilityAndEmitFlags = 2816
        | ContextFlags = 25358336
        | TypeExcludesFlags = 40960

    type [<RequireQualifiedAccess>] ModifierFlags =
        | None = 0
        | Export = 1
        | Ambient = 2
        | Public = 4
        | Private = 8
        | Protected = 16
        | Static = 32
        | Readonly = 64
        | Abstract = 128
        | Async = 256
        | Default = 512
        | Const = 2048
        | HasComputedJSDocModifiers = 4096
        | Deprecated = 8192
        | Override = 16384
        | HasComputedFlags = 536870912
        | AccessibilityModifier = 28
        | ParameterPropertyModifier = 16476
        | NonPublicAccessibilityModifier = 24
        | TypeScriptModifier = 18654
        | ExportDefault = 513
        | All = 27647

    type [<RequireQualifiedAccess>] JsxFlags =
        | None = 0
        /// An element from a named property of the JSX.IntrinsicElements interface
        | IntrinsicNamedElement = 1
        /// An element inferred from the string index signature of the JSX.IntrinsicElements interface
        | IntrinsicIndexedElement = 2
        | IntrinsicElement = 3

    type [<AllowNullLiteral>] Node =
        inherit ReadonlyTextRange
        abstract kind: SyntaxKind
        abstract flags: NodeFlags
        abstract decorators: Decorator[] option
        abstract modifiers: ModifiersArray option
        abstract parent: Node
        abstract getSourceFile: unit -> SourceFile
        abstract getChildCount: ?sourceFile: SourceFile -> float
        abstract getChildAt: index: float * ?sourceFile: SourceFile -> Node
        abstract getChildren: ?sourceFile: SourceFile -> Node[]
        abstract getStart: ?sourceFile: SourceFile * ?includeJsDocComment: bool -> float
        abstract getFullStart: unit -> float
        abstract getEnd: unit -> float
        abstract getWidth: ?sourceFile: SourceFileLike -> float
        abstract getFullWidth: unit -> float
        abstract getLeadingTriviaWidth: ?sourceFile: SourceFile -> float
        abstract getFullText: ?sourceFile: SourceFile -> string
        abstract getText: ?sourceFile: SourceFile -> string
        abstract getFirstToken: ?sourceFile: SourceFile -> Node option
        abstract getLastToken: ?sourceFile: SourceFile -> Node option
        abstract forEachChild: cbNode: (Node -> 'T option) * ?cbNodeArray: (Node[] -> 'T option) -> 'T option

    type [<AllowNullLiteral>] JSDocContainer =
        interface end

    type HasJSDoc =
        obj

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] HasType =
        | [<CompiledValue 163>] ParameterDeclaration of ParameterDeclaration
        | [<CompiledValue 165>] PropertySignature of PropertySignature
        | [<CompiledValue 166>] PropertyDeclaration of PropertyDeclaration
        | [<CompiledValue 167>] MethodSignature of MethodSignature
        | [<CompiledValue 168>] MethodDeclaration of MethodDeclaration
        | [<CompiledValue 170>] ConstructorDeclaration of ConstructorDeclaration
        | [<CompiledValue 171>] GetAccessorDeclaration of GetAccessorDeclaration
        | [<CompiledValue 172>] SetAccessorDeclaration of SetAccessorDeclaration
        | [<CompiledValue 173>] CallSignatureDeclaration of CallSignatureDeclaration
        | [<CompiledValue 174>] ConstructSignatureDeclaration of ConstructSignatureDeclaration
        | [<CompiledValue 175>] IndexSignatureDeclaration of IndexSignatureDeclaration
        | [<CompiledValue 176>] TypePredicateNode of TypePredicateNode
        | [<CompiledValue 178>] FunctionTypeNode of FunctionTypeNode
        | [<CompiledValue 179>] ConstructorTypeNode of ConstructorTypeNode
        | [<CompiledValue 190>] ParenthesizedTypeNode of ParenthesizedTypeNode
        | [<CompiledValue 192>] TypeOperatorNode of TypeOperatorNode
        | [<CompiledValue 194>] MappedTypeNode of MappedTypeNode
        | [<CompiledValue 210>] TypeAssertion of TypeAssertion
        | [<CompiledValue 212>] FunctionExpression of FunctionExpression
        | [<CompiledValue 213>] ArrowFunction of ArrowFunction
        | [<CompiledValue 228>] AsExpression of AsExpression
        | [<CompiledValue 253>] VariableDeclaration of VariableDeclaration
        | [<CompiledValue 255>] FunctionDeclaration of FunctionDeclaration
        | [<CompiledValue 258>] TypeAliasDeclaration of TypeAliasDeclaration
        | [<CompiledValue 307>] JSDocTypeExpression of JSDocTypeExpression
        | [<CompiledValue 312>] JSDocNullableType of JSDocNullableType
        | [<CompiledValue 313>] JSDocNonNullableType of JSDocNonNullableType
        | [<CompiledValue 314>] JSDocOptionalType of JSDocOptionalType
        | [<CompiledValue 315>] JSDocFunctionType of JSDocFunctionType
        | [<CompiledValue 316>] JSDocVariadicType of JSDocVariadicType

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] HasTypeArguments =
        | [<CompiledValue 207>] CallExpression of CallExpression
        | [<CompiledValue 208>] NewExpression of NewExpression
        | [<CompiledValue 209>] TaggedTemplateExpression of TaggedTemplateExpression
        | [<CompiledValue 278>] JsxSelfClosingElement of JsxSelfClosingElement
        | [<CompiledValue 279>] JsxOpeningElement of JsxOpeningElement

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] HasInitializer =
        | [<CompiledValue 163>] ParameterDeclaration of ParameterDeclaration
        | [<CompiledValue 165>] PropertySignature of PropertySignature
        | [<CompiledValue 166>] PropertyDeclaration of PropertyDeclaration
        | [<CompiledValue 202>] BindingElement of BindingElement
        | [<CompiledValue 241>] ForStatement of ForStatement
        | [<CompiledValue 242>] ForInStatement of ForInStatement
        | [<CompiledValue 243>] ForOfStatement of ForOfStatement
        | [<CompiledValue 253>] VariableDeclaration of VariableDeclaration
        | [<CompiledValue 284>] JsxAttribute of JsxAttribute
        | [<CompiledValue 294>] PropertyAssignment of PropertyAssignment
        | [<CompiledValue 297>] EnumMember of EnumMember

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] HasExpressionInitializer =
        | [<CompiledValue 163>] ParameterDeclaration of ParameterDeclaration
        | [<CompiledValue 165>] PropertySignature of PropertySignature
        | [<CompiledValue 166>] PropertyDeclaration of PropertyDeclaration
        | [<CompiledValue 202>] BindingElement of BindingElement
        | [<CompiledValue 253>] VariableDeclaration of VariableDeclaration
        | [<CompiledValue 294>] PropertyAssignment of PropertyAssignment
        | [<CompiledValue 297>] EnumMember of EnumMember

    type [<AllowNullLiteral>] NodeArray<'T> =
        inherit ReadonlyArray<'T>
        inherit ReadonlyTextRange
        abstract hasTrailingComma: bool

    type [<AllowNullLiteral>] Token<'TKind> =
        inherit Node
        abstract kind: 'TKind

    type [<AllowNullLiteral>] EndOfFileToken =
        interface end

    type [<AllowNullLiteral>] PunctuationToken<'TKind> =
        inherit Token<'TKind>

    type DotToken =
        PunctuationToken<SyntaxKind>

    type DotDotDotToken =
        PunctuationToken<SyntaxKind>

    type QuestionToken =
        PunctuationToken<SyntaxKind>

    type ExclamationToken =
        PunctuationToken<SyntaxKind>

    type ColonToken =
        PunctuationToken<SyntaxKind>

    type EqualsToken =
        PunctuationToken<SyntaxKind>

    type AsteriskToken =
        PunctuationToken<SyntaxKind>

    type EqualsGreaterThanToken =
        PunctuationToken<SyntaxKind>

    type PlusToken =
        PunctuationToken<SyntaxKind>

    type MinusToken =
        PunctuationToken<SyntaxKind>

    type QuestionDotToken =
        PunctuationToken<SyntaxKind>

    type [<AllowNullLiteral>] KeywordToken<'TKind> =
        inherit Token<'TKind>

    type AssertsKeyword =
        KeywordToken<SyntaxKind>

    type AssertKeyword =
        KeywordToken<SyntaxKind>

    type AwaitKeyword =
        KeywordToken<SyntaxKind>

    [<Obsolete("Use `AwaitKeyword` instead.")>]
    type AwaitKeywordToken =
        AwaitKeyword

    [<Obsolete("Use `AssertsKeyword` instead.")>]
    type AssertsToken =
        AssertsKeyword

    type [<AllowNullLiteral>] ModifierToken<'TKind> =
        inherit KeywordToken<'TKind>

    type AbstractKeyword =
        ModifierToken<SyntaxKind>

    type AsyncKeyword =
        ModifierToken<SyntaxKind>

    type ConstKeyword =
        ModifierToken<SyntaxKind>

    type DeclareKeyword =
        ModifierToken<SyntaxKind>

    type DefaultKeyword =
        ModifierToken<SyntaxKind>

    type ExportKeyword =
        ModifierToken<SyntaxKind>

    type PrivateKeyword =
        ModifierToken<SyntaxKind>

    type ProtectedKeyword =
        ModifierToken<SyntaxKind>

    type PublicKeyword =
        ModifierToken<SyntaxKind>

    type ReadonlyKeyword =
        ModifierToken<SyntaxKind>

    type OverrideKeyword =
        ModifierToken<SyntaxKind>

    type StaticKeyword =
        ModifierToken<SyntaxKind>

    [<Obsolete("Use `ReadonlyKeyword` instead.")>]
    type ReadonlyToken =
        ReadonlyKeyword

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] Modifier =
        | [<CompiledValue 85>] ConstKeyword of ConstKeyword
        | [<CompiledValue 88>] DefaultKeyword of DefaultKeyword
        | [<CompiledValue 93>] ExportKeyword of ExportKeyword
        | [<CompiledValue 121>] PrivateKeyword of PrivateKeyword
        | [<CompiledValue 122>] ProtectedKeyword of ProtectedKeyword
        | [<CompiledValue 123>] PublicKeyword of PublicKeyword
        | [<CompiledValue 124>] StaticKeyword of StaticKeyword
        | [<CompiledValue 126>] AbstractKeyword of AbstractKeyword
        | [<CompiledValue 131>] AsyncKeyword of AsyncKeyword
        | [<CompiledValue 135>] DeclareKeyword of DeclareKeyword
        | [<CompiledValue 144>] ReadonlyKeyword of ReadonlyKeyword
        | [<CompiledValue 158>] OverrideKeyword of OverrideKeyword
    with
        [<Emit("$0.kind")>] member this.kind : SyntaxKind = jsNative

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] AccessibilityModifier =
        | [<CompiledValue 121>] PrivateKeyword of PrivateKeyword
        | [<CompiledValue 122>] ProtectedKeyword of ProtectedKeyword
        | [<CompiledValue 123>] PublicKeyword of PublicKeyword

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ParameterPropertyModifier =
        | [<CompiledValue 121>] PrivateKeyword of PrivateKeyword
        | [<CompiledValue 122>] ProtectedKeyword of ProtectedKeyword
        | [<CompiledValue 123>] PublicKeyword of PublicKeyword
        | [<CompiledValue 144>] ReadonlyKeyword of ReadonlyKeyword

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ClassMemberModifier =
        | [<CompiledValue 121>] PrivateKeyword of PrivateKeyword
        | [<CompiledValue 122>] ProtectedKeyword of ProtectedKeyword
        | [<CompiledValue 123>] PublicKeyword of PublicKeyword
        | [<CompiledValue 124>] StaticKeyword of StaticKeyword
        | [<CompiledValue 144>] ReadonlyKeyword of ReadonlyKeyword

    type ModifiersArray =
        Modifier[]

    type [<RequireQualifiedAccess>] GeneratedIdentifierFlags =
        | None = 0
        | ReservedInNestedScopes = 8
        | Optimistic = 16
        | FileLevel = 32
        | AllowNameSubstitution = 64

    type [<AllowNullLiteral>] Identifier =
        inherit PrimaryExpression
        inherit Declaration
        abstract kind: SyntaxKind
        /// <summary>
        /// Prefer to use <c>id.unescapedText</c>. (Note: This is available only in services, not internally to the TypeScript compiler.)
        /// Text of identifier, but if the identifier begins with two underscores, this will begin with three.
        /// </summary>
        abstract escapedText: __String
        abstract originalKeywordKind: SyntaxKind option
        abstract isInJSDocNamespace: bool option with get, set
        abstract text: string

    type [<AllowNullLiteral>] TransientIdentifier =
        inherit Identifier
        abstract resolvedSymbol: Symbol with get, set

    type [<AllowNullLiteral>] QualifiedName =
        inherit Node
        abstract kind: SyntaxKind
        abstract left: EntityName
        abstract right: Identifier

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] EntityName =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 160>] QualifiedName of QualifiedName

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] PropertyName =
        | [<CompiledValue 8>] NumericLiteral of NumericLiteral
        | [<CompiledValue 10>] StringLiteral of StringLiteral
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 80>] PrivateIdentifier of PrivateIdentifier
        | [<CompiledValue 161>] ComputedPropertyName of ComputedPropertyName

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] MemberName =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 80>] PrivateIdentifier of PrivateIdentifier

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] DeclarationName =
        | [<CompiledValue 8>] NumericLiteral of NumericLiteral
        | [<CompiledValue 10>] StringLiteral of StringLiteral
        | [<CompiledValue 14>] NoSubstitutionTemplateLiteral of NoSubstitutionTemplateLiteral
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 80>] PrivateIdentifier of PrivateIdentifier
        | [<CompiledValue 161>] ComputedPropertyName of ComputedPropertyName
        | [<CompiledValue 200>] ObjectBindingPattern of ObjectBindingPattern
        | [<CompiledValue 201>] ArrayBindingPattern of ArrayBindingPattern
        | [<CompiledValue 205>] PropertyAccessEntityNameExpression of PropertyAccessEntityNameExpression
        | [<CompiledValue 206>] ElementAccessExpression of ElementAccessExpression

    type [<AllowNullLiteral>] Declaration =
        inherit Node
        abstract _declarationBrand: obj option with get, set

    type [<AllowNullLiteral>] NamedDeclaration =
        inherit Declaration
        abstract name: DeclarationName option

    type [<AllowNullLiteral>] DeclarationStatement =
        inherit NamedDeclaration
        inherit Statement
        abstract name: U3<Identifier, StringLiteral, NumericLiteral> option

    type [<AllowNullLiteral>] ComputedPropertyName =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: Declaration
        abstract expression: Expression

    type [<AllowNullLiteral>] PrivateIdentifier =
        inherit PrimaryExpression
        abstract kind: SyntaxKind
        abstract escapedText: __String
        abstract text: string

    type [<AllowNullLiteral>] Decorator =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: NamedDeclaration
        abstract expression: LeftHandSideExpression

    type [<AllowNullLiteral>] TypeParameterDeclaration =
        inherit NamedDeclaration
        abstract kind: SyntaxKind
        abstract parent: U2<DeclarationWithTypeParameterChildren, InferTypeNode>
        abstract name: Identifier
        /// <summary>Note: Consider calling <c>getEffectiveConstraintOfTypeParameter</c></summary>
        abstract ``constraint``: TypeNode option
        abstract ``default``: TypeNode option
        abstract expression: Expression option with get, set

    type [<AllowNullLiteral>] SignatureDeclarationBase =
        inherit NamedDeclaration
        inherit JSDocContainer
        abstract kind: obj
        abstract name: PropertyName option
        abstract typeParameters: TypeParameterDeclaration[] option
        abstract parameters: ParameterDeclaration[]
        abstract ``type``: TypeNode option

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] SignatureDeclaration =
        | [<CompiledValue 167>] MethodSignature of MethodSignature
        | [<CompiledValue 168>] MethodDeclaration of MethodDeclaration
        | [<CompiledValue 170>] ConstructorDeclaration of ConstructorDeclaration
        | [<CompiledValue 171>] GetAccessorDeclaration of GetAccessorDeclaration
        | [<CompiledValue 172>] SetAccessorDeclaration of SetAccessorDeclaration
        | [<CompiledValue 173>] CallSignatureDeclaration of CallSignatureDeclaration
        | [<CompiledValue 174>] ConstructSignatureDeclaration of ConstructSignatureDeclaration
        | [<CompiledValue 175>] IndexSignatureDeclaration of IndexSignatureDeclaration
        | [<CompiledValue 178>] FunctionTypeNode of FunctionTypeNode
        | [<CompiledValue 179>] ConstructorTypeNode of ConstructorTypeNode
        | [<CompiledValue 212>] FunctionExpression of FunctionExpression
        | [<CompiledValue 213>] ArrowFunction of ArrowFunction
        | [<CompiledValue 255>] FunctionDeclaration of FunctionDeclaration
        | [<CompiledValue 315>] JSDocFunctionType of JSDocFunctionType

    type [<AllowNullLiteral>] CallSignatureDeclaration =
        inherit SignatureDeclarationBase
        inherit TypeElement
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] ConstructSignatureDeclaration =
        inherit SignatureDeclarationBase
        inherit TypeElement
        abstract kind: SyntaxKind

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] BindingName =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 200>] ObjectBindingPattern of ObjectBindingPattern
        | [<CompiledValue 201>] ArrayBindingPattern of ArrayBindingPattern

    type [<AllowNullLiteral>] VariableDeclaration =
        inherit NamedDeclaration
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: U2<VariableDeclarationList, CatchClause>
        abstract name: BindingName
        abstract exclamationToken: ExclamationToken option
        abstract ``type``: TypeNode option
        abstract initializer: Expression option

    type [<AllowNullLiteral>] VariableDeclarationList =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: U4<VariableStatement, ForStatement, ForOfStatement, ForInStatement>
        abstract declarations: VariableDeclaration[]

    type [<AllowNullLiteral>] ParameterDeclaration =
        inherit NamedDeclaration
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: SignatureDeclaration
        abstract dotDotDotToken: DotDotDotToken option
        abstract name: BindingName
        abstract questionToken: QuestionToken option
        abstract ``type``: TypeNode option
        abstract initializer: Expression option

    type [<AllowNullLiteral>] BindingElement =
        inherit NamedDeclaration
        abstract kind: SyntaxKind
        abstract parent: BindingPattern
        abstract propertyName: PropertyName option
        abstract dotDotDotToken: DotDotDotToken option
        abstract name: BindingName
        abstract initializer: Expression option

    type [<AllowNullLiteral>] PropertySignature =
        inherit TypeElement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract name: PropertyName
        abstract questionToken: QuestionToken option
        abstract ``type``: TypeNode option
        abstract initializer: Expression option with get, set

    type [<AllowNullLiteral>] PropertyDeclaration =
        inherit ClassElement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: ClassLikeDeclaration
        abstract name: PropertyName
        abstract questionToken: QuestionToken option
        abstract exclamationToken: ExclamationToken option
        abstract ``type``: TypeNode option
        abstract initializer: Expression option

    type [<AllowNullLiteral>] ObjectLiteralElement =
        inherit NamedDeclaration
        abstract _objectLiteralBrand: obj option with get, set
        abstract name: PropertyName option

    /// Unlike ObjectLiteralElement, excludes JSXAttribute and JSXSpreadAttribute.
    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ObjectLiteralElementLike =
        | [<CompiledValue 168>] MethodDeclaration of MethodDeclaration
        | [<CompiledValue 171>] GetAccessorDeclaration of GetAccessorDeclaration
        | [<CompiledValue 172>] SetAccessorDeclaration of SetAccessorDeclaration
        | [<CompiledValue 294>] PropertyAssignment of PropertyAssignment
        | [<CompiledValue 295>] ShorthandPropertyAssignment of ShorthandPropertyAssignment
        | [<CompiledValue 296>] SpreadAssignment of SpreadAssignment

    type [<AllowNullLiteral>] PropertyAssignment =
        inherit ObjectLiteralElement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: ObjectLiteralExpression
        abstract name: PropertyName
        abstract questionToken: QuestionToken option
        abstract exclamationToken: ExclamationToken option
        abstract initializer: Expression

    type [<AllowNullLiteral>] ShorthandPropertyAssignment =
        inherit ObjectLiteralElement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: ObjectLiteralExpression
        abstract name: Identifier
        abstract questionToken: QuestionToken option
        abstract exclamationToken: ExclamationToken option
        abstract equalsToken: EqualsToken option
        abstract objectAssignmentInitializer: Expression option

    type [<AllowNullLiteral>] SpreadAssignment =
        inherit ObjectLiteralElement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: ObjectLiteralExpression
        abstract expression: Expression

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] VariableLikeDeclaration =
        | [<CompiledValue 163>] ParameterDeclaration of ParameterDeclaration
        | [<CompiledValue 165>] PropertySignature of PropertySignature
        | [<CompiledValue 166>] PropertyDeclaration of PropertyDeclaration
        | [<CompiledValue 202>] BindingElement of BindingElement
        | [<CompiledValue 253>] VariableDeclaration of VariableDeclaration
        | [<CompiledValue 284>] JsxAttribute of JsxAttribute
        | [<CompiledValue 294>] PropertyAssignment of PropertyAssignment
        | [<CompiledValue 295>] ShorthandPropertyAssignment of ShorthandPropertyAssignment
        | [<CompiledValue 297>] EnumMember of EnumMember
        | [<CompiledValue 338>] JSDocParameterTag of JSDocParameterTag
        | [<CompiledValue 345>] JSDocPropertyTag of JSDocPropertyTag

    type [<AllowNullLiteral>] PropertyLikeDeclaration =
        inherit NamedDeclaration
        abstract name: PropertyName

    type [<AllowNullLiteral>] ObjectBindingPattern =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: U3<VariableDeclaration, ParameterDeclaration, BindingElement>
        abstract elements: BindingElement[]

    type [<AllowNullLiteral>] ArrayBindingPattern =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: U3<VariableDeclaration, ParameterDeclaration, BindingElement>
        abstract elements: ArrayBindingElement[]

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] BindingPattern =
        | [<CompiledValue 200>] ObjectBindingPattern of ObjectBindingPattern
        | [<CompiledValue 201>] ArrayBindingPattern of ArrayBindingPattern

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ArrayBindingElement =
        | [<CompiledValue 202>] BindingElement of BindingElement
        | [<CompiledValue 226>] OmittedExpression of OmittedExpression

    /// Several node kinds share function-like features such as a signature,
    /// a name, and a body. These nodes should extend FunctionLikeDeclarationBase.
    /// Examples:
    /// - FunctionDeclaration
    /// - MethodDeclaration
    /// - AccessorDeclaration
    type [<AllowNullLiteral>] FunctionLikeDeclarationBase =
        inherit SignatureDeclarationBase
        abstract _functionLikeDeclarationBrand: obj option with get, set
        abstract asteriskToken: AsteriskToken option
        abstract questionToken: QuestionToken option
        abstract exclamationToken: ExclamationToken option
        abstract body: U2<Block, Expression> option

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] FunctionLikeDeclaration =
        | [<CompiledValue 168>] MethodDeclaration of MethodDeclaration
        | [<CompiledValue 170>] ConstructorDeclaration of ConstructorDeclaration
        | [<CompiledValue 171>] GetAccessorDeclaration of GetAccessorDeclaration
        | [<CompiledValue 172>] SetAccessorDeclaration of SetAccessorDeclaration
        | [<CompiledValue 212>] FunctionExpression of FunctionExpression
        | [<CompiledValue 213>] ArrowFunction of ArrowFunction
        | [<CompiledValue 255>] FunctionDeclaration of FunctionDeclaration

    [<Obsolete("Use SignatureDeclaration")>]
    type FunctionLike =
        SignatureDeclaration

    type [<AllowNullLiteral>] FunctionDeclaration =
        inherit FunctionLikeDeclarationBase
        inherit DeclarationStatement
        abstract kind: SyntaxKind
        abstract name: Identifier option
        abstract body: FunctionBody option

    type [<AllowNullLiteral>] MethodSignature =
        inherit SignatureDeclarationBase
        inherit TypeElement
        abstract kind: SyntaxKind
        abstract parent: ObjectTypeDeclaration
        abstract name: PropertyName

    type [<AllowNullLiteral>] MethodDeclaration =
        inherit FunctionLikeDeclarationBase
        inherit ClassElement
        inherit ObjectLiteralElement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: U2<ClassLikeDeclaration, ObjectLiteralExpression>
        abstract name: PropertyName
        abstract body: FunctionBody option

    type [<AllowNullLiteral>] ConstructorDeclaration =
        inherit FunctionLikeDeclarationBase
        inherit ClassElement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: ClassLikeDeclaration
        abstract body: FunctionBody option

    /// For when we encounter a semicolon in a class declaration. ES6 allows these as class elements.
    type [<AllowNullLiteral>] SemicolonClassElement =
        inherit ClassElement
        abstract kind: SyntaxKind
        abstract parent: ClassLikeDeclaration

    type [<AllowNullLiteral>] GetAccessorDeclaration =
        inherit FunctionLikeDeclarationBase
        inherit ClassElement
        inherit TypeElement
        inherit ObjectLiteralElement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: U4<ClassLikeDeclaration, ObjectLiteralExpression, TypeLiteralNode, InterfaceDeclaration>
        abstract name: PropertyName
        abstract body: FunctionBody option

    type [<AllowNullLiteral>] SetAccessorDeclaration =
        inherit FunctionLikeDeclarationBase
        inherit ClassElement
        inherit TypeElement
        inherit ObjectLiteralElement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: U4<ClassLikeDeclaration, ObjectLiteralExpression, TypeLiteralNode, InterfaceDeclaration>
        abstract name: PropertyName
        abstract body: FunctionBody option

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] AccessorDeclaration =
        | [<CompiledValue 171>] GetAccessorDeclaration of GetAccessorDeclaration
        | [<CompiledValue 172>] SetAccessorDeclaration of SetAccessorDeclaration

    type [<AllowNullLiteral>] IndexSignatureDeclaration =
        inherit SignatureDeclarationBase
        inherit ClassElement
        inherit TypeElement
        abstract kind: SyntaxKind
        abstract parent: ObjectTypeDeclaration
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] ClassStaticBlockDeclaration =
        inherit ClassElement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: U2<ClassDeclaration, ClassExpression>
        abstract body: Block

    type [<AllowNullLiteral>] TypeNode =
        inherit Node
        abstract _typeNodeBrand: obj option with get, set

    type KeywordTypeNode =
        KeywordTypeNode<KeywordTypeSyntaxKind>

    type [<AllowNullLiteral>] KeywordTypeNode<'TKind> =
        inherit KeywordToken<'TKind>
        inherit TypeNode
        abstract kind: 'TKind

    type [<AllowNullLiteral>] ImportTypeNode =
        inherit NodeWithTypeArguments
        abstract kind: SyntaxKind
        abstract isTypeOf: bool
        abstract argument: TypeNode
        abstract qualifier: EntityName option

    type [<AllowNullLiteral>] ThisTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] FunctionOrConstructorTypeNode =
        | [<CompiledValue 178>] FunctionTypeNode of FunctionTypeNode
        | [<CompiledValue 179>] ConstructorTypeNode of ConstructorTypeNode

    type [<AllowNullLiteral>] FunctionOrConstructorTypeNodeBase =
        inherit TypeNode
        inherit SignatureDeclarationBase
        abstract kind: SyntaxKind
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] FunctionTypeNode =
        inherit FunctionOrConstructorTypeNodeBase
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] ConstructorTypeNode =
        inherit FunctionOrConstructorTypeNodeBase
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] NodeWithTypeArguments =
        inherit TypeNode
        abstract typeArguments: TypeNode[] option

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] TypeReferenceType =
        | [<CompiledValue 177>] TypeReferenceNode of TypeReferenceNode
        | [<CompiledValue 227>] ExpressionWithTypeArguments of ExpressionWithTypeArguments

    type [<AllowNullLiteral>] TypeReferenceNode =
        inherit NodeWithTypeArguments
        abstract kind: SyntaxKind
        abstract typeName: EntityName

    type [<AllowNullLiteral>] TypePredicateNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract parent: U2<SignatureDeclaration, JSDocTypeExpression>
        abstract assertsModifier: AssertsKeyword option
        abstract parameterName: U2<Identifier, ThisTypeNode>
        abstract ``type``: TypeNode option

    type [<AllowNullLiteral>] TypeQueryNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract exprName: EntityName

    type [<AllowNullLiteral>] TypeLiteralNode =
        inherit TypeNode
        inherit Declaration
        abstract kind: SyntaxKind
        abstract members: TypeElement[]

    type [<AllowNullLiteral>] ArrayTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract elementType: TypeNode

    type [<AllowNullLiteral>] TupleTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract elements: U2<TypeNode, NamedTupleMember>[]

    type [<AllowNullLiteral>] NamedTupleMember =
        inherit TypeNode
        inherit JSDocContainer
        inherit Declaration
        abstract kind: SyntaxKind
        abstract dotDotDotToken: Token<SyntaxKind> option
        abstract name: Identifier
        abstract questionToken: Token<SyntaxKind> option
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] OptionalTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] RestTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract ``type``: TypeNode

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] UnionOrIntersectionTypeNode =
        | [<CompiledValue 186>] UnionTypeNode of UnionTypeNode
        | [<CompiledValue 187>] IntersectionTypeNode of IntersectionTypeNode

    type [<AllowNullLiteral>] UnionTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract types: TypeNode[]

    type [<AllowNullLiteral>] IntersectionTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract types: TypeNode[]

    type [<AllowNullLiteral>] ConditionalTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract checkType: TypeNode
        abstract extendsType: TypeNode
        abstract trueType: TypeNode
        abstract falseType: TypeNode

    type [<AllowNullLiteral>] InferTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract typeParameter: TypeParameterDeclaration

    type [<AllowNullLiteral>] ParenthesizedTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] TypeOperatorNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract operator: SyntaxKind
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] IndexedAccessTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract objectType: TypeNode
        abstract indexType: TypeNode

    type [<AllowNullLiteral>] MappedTypeNode =
        inherit TypeNode
        inherit Declaration
        abstract kind: SyntaxKind
        abstract readonlyToken: U3<ReadonlyKeyword, PlusToken, MinusToken> option
        abstract typeParameter: TypeParameterDeclaration
        abstract nameType: TypeNode option
        abstract questionToken: U3<QuestionToken, PlusToken, MinusToken> option
        abstract ``type``: TypeNode option
        /// Used only to produce grammar errors
        abstract members: TypeElement[] option

    type [<AllowNullLiteral>] LiteralTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract literal: U4<NullLiteral, BooleanLiteral, LiteralExpression, PrefixUnaryExpression>

    type [<AllowNullLiteral>] StringLiteral =
        inherit LiteralExpression
        inherit Declaration
        abstract kind: SyntaxKind

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] StringLiteralLike =
        | [<CompiledValue 10>] StringLiteral of StringLiteral
        | [<CompiledValue 14>] NoSubstitutionTemplateLiteral of NoSubstitutionTemplateLiteral

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] PropertyNameLiteral =
        | [<CompiledValue 8>] NumericLiteral of NumericLiteral
        | [<CompiledValue 10>] StringLiteral of StringLiteral
        | [<CompiledValue 14>] NoSubstitutionTemplateLiteral of NoSubstitutionTemplateLiteral
        | [<CompiledValue 79>] Identifier of Identifier

    type [<AllowNullLiteral>] TemplateLiteralTypeNode =
        inherit TypeNode
        abstract kind: SyntaxKind with get, set
        abstract head: TemplateHead
        abstract templateSpans: TemplateLiteralTypeSpan[]

    type [<AllowNullLiteral>] TemplateLiteralTypeSpan =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract parent: TemplateLiteralTypeNode
        abstract ``type``: TypeNode
        abstract literal: U2<TemplateMiddle, TemplateTail>

    type [<AllowNullLiteral>] Expression =
        inherit Node
        abstract _expressionBrand: obj option with get, set

    type [<AllowNullLiteral>] OmittedExpression =
        inherit Expression
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] PartiallyEmittedExpression =
        inherit LeftHandSideExpression
        abstract kind: SyntaxKind
        abstract expression: Expression

    type [<AllowNullLiteral>] UnaryExpression =
        inherit Expression
        abstract _unaryExpressionBrand: obj option with get, set

    /// Deprecated, please use UpdateExpression
    type IncrementExpression =
        UpdateExpression

    type [<AllowNullLiteral>] UpdateExpression =
        inherit UnaryExpression
        abstract _updateExpressionBrand: obj option with get, set

    type PrefixUnaryOperator =
        SyntaxKind

    type [<AllowNullLiteral>] PrefixUnaryExpression =
        inherit UpdateExpression
        abstract kind: SyntaxKind
        abstract operator: PrefixUnaryOperator
        abstract operand: UnaryExpression

    type PostfixUnaryOperator =
        SyntaxKind

    type [<AllowNullLiteral>] PostfixUnaryExpression =
        inherit UpdateExpression
        abstract kind: SyntaxKind
        abstract operand: LeftHandSideExpression
        abstract operator: PostfixUnaryOperator

    type [<AllowNullLiteral>] LeftHandSideExpression =
        inherit UpdateExpression
        abstract _leftHandSideExpressionBrand: obj option with get, set

    type [<AllowNullLiteral>] MemberExpression =
        inherit LeftHandSideExpression
        abstract _memberExpressionBrand: obj option with get, set

    type [<AllowNullLiteral>] PrimaryExpression =
        inherit MemberExpression
        abstract _primaryExpressionBrand: obj option with get, set

    type [<AllowNullLiteral>] NullLiteral =
        inherit PrimaryExpression
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] TrueLiteral =
        inherit PrimaryExpression
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] FalseLiteral =
        inherit PrimaryExpression
        abstract kind: SyntaxKind

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] BooleanLiteral =
        | [<CompiledValue 95>] FalseLiteral of FalseLiteral
        | [<CompiledValue 110>] TrueLiteral of TrueLiteral

    type [<AllowNullLiteral>] ThisExpression =
        inherit PrimaryExpression
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] SuperExpression =
        inherit PrimaryExpression
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] ImportExpression =
        inherit PrimaryExpression
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] DeleteExpression =
        inherit UnaryExpression
        abstract kind: SyntaxKind
        abstract expression: UnaryExpression

    type [<AllowNullLiteral>] TypeOfExpression =
        inherit UnaryExpression
        abstract kind: SyntaxKind
        abstract expression: UnaryExpression

    type [<AllowNullLiteral>] VoidExpression =
        inherit UnaryExpression
        abstract kind: SyntaxKind
        abstract expression: UnaryExpression

    type [<AllowNullLiteral>] AwaitExpression =
        inherit UnaryExpression
        abstract kind: SyntaxKind
        abstract expression: UnaryExpression

    type [<AllowNullLiteral>] YieldExpression =
        inherit Expression
        abstract kind: SyntaxKind
        abstract asteriskToken: AsteriskToken option
        abstract expression: Expression option

    type [<AllowNullLiteral>] SyntheticExpression =
        inherit Expression
        abstract kind: SyntaxKind
        abstract isSpread: bool
        abstract ``type``: Type
        abstract tupleNameSource: U2<ParameterDeclaration, NamedTupleMember> option

    type ExponentiationOperator =
        SyntaxKind

    type MultiplicativeOperator =
        SyntaxKind

    type MultiplicativeOperatorOrHigher =
        U2<ExponentiationOperator, MultiplicativeOperator>

    type AdditiveOperator =
        SyntaxKind

    type AdditiveOperatorOrHigher =
        U2<MultiplicativeOperatorOrHigher, AdditiveOperator>

    type ShiftOperator =
        SyntaxKind

    type ShiftOperatorOrHigher =
        U2<AdditiveOperatorOrHigher, ShiftOperator>

    type RelationalOperator =
        SyntaxKind

    type RelationalOperatorOrHigher =
        U2<ShiftOperatorOrHigher, RelationalOperator>

    type EqualityOperator =
        SyntaxKind

    type EqualityOperatorOrHigher =
        U2<RelationalOperatorOrHigher, EqualityOperator>

    type BitwiseOperator =
        SyntaxKind

    type BitwiseOperatorOrHigher =
        U2<EqualityOperatorOrHigher, BitwiseOperator>

    type LogicalOperator =
        SyntaxKind

    type LogicalOperatorOrHigher =
        U2<BitwiseOperatorOrHigher, LogicalOperator>

    type CompoundAssignmentOperator =
        SyntaxKind

    type AssignmentOperator =
        U2<SyntaxKind, CompoundAssignmentOperator>

    type AssignmentOperatorOrHigher =
        U3<SyntaxKind, LogicalOperatorOrHigher, AssignmentOperator>

    type BinaryOperator =
        U2<AssignmentOperatorOrHigher, SyntaxKind>

    type LogicalOrCoalescingAssignmentOperator =
        SyntaxKind

    type BinaryOperatorToken =
        Token<BinaryOperator>

    type [<AllowNullLiteral>] BinaryExpression =
        inherit Expression
        inherit Declaration
        abstract kind: SyntaxKind
        abstract left: Expression
        abstract operatorToken: BinaryOperatorToken
        abstract right: Expression

    type AssignmentOperatorToken =
        Token<AssignmentOperator>

    type [<AllowNullLiteral>] AssignmentExpression<'TOperator> =
        inherit BinaryExpression
        abstract left: LeftHandSideExpression
        abstract operatorToken: 'TOperator

    type [<AllowNullLiteral>] ObjectDestructuringAssignment =
        inherit AssignmentExpression<EqualsToken>
        abstract left: ObjectLiteralExpression

    type [<AllowNullLiteral>] ArrayDestructuringAssignment =
        inherit AssignmentExpression<EqualsToken>
        abstract left: ArrayLiteralExpression

    type DestructuringAssignment =
        U2<ObjectDestructuringAssignment, ArrayDestructuringAssignment>

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] BindingOrAssignmentElement =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 163>] ParameterDeclaration of ParameterDeclaration
        | [<CompiledValue 202>] BindingElement of BindingElement
        | [<CompiledValue 203>] ArrayLiteralExpression of ArrayLiteralExpression
        | [<CompiledValue 204>] ObjectLiteralExpression of ObjectLiteralExpression
        | [<CompiledValue 205>] PropertyAccessExpression of PropertyAccessExpression
        | [<CompiledValue 206>] ElementAccessExpression of ElementAccessExpression
        | [<CompiledValue 220>] AssignmentExpression of AssignmentExpression<EqualsToken>
        | [<CompiledValue 224>] SpreadElement of SpreadElement
        | [<CompiledValue 226>] OmittedExpression of OmittedExpression
        | [<CompiledValue 253>] VariableDeclaration of VariableDeclaration
        | [<CompiledValue 294>] PropertyAssignment of PropertyAssignment
        | [<CompiledValue 295>] ShorthandPropertyAssignment of ShorthandPropertyAssignment
        | [<CompiledValue 296>] SpreadAssignment of SpreadAssignment

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ObjectBindingOrAssignmentElement =
        | [<CompiledValue 202>] BindingElement of BindingElement
        | [<CompiledValue 294>] PropertyAssignment of PropertyAssignment
        | [<CompiledValue 295>] ShorthandPropertyAssignment of ShorthandPropertyAssignment
        | [<CompiledValue 296>] SpreadAssignment of SpreadAssignment

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ArrayBindingOrAssignmentElement =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 202>] BindingElement of BindingElement
        | [<CompiledValue 203>] ArrayLiteralExpression of ArrayLiteralExpression
        | [<CompiledValue 204>] ObjectLiteralExpression of ObjectLiteralExpression
        | [<CompiledValue 205>] PropertyAccessExpression of PropertyAccessExpression
        | [<CompiledValue 206>] ElementAccessExpression of ElementAccessExpression
        | [<CompiledValue 220>] AssignmentExpression of AssignmentExpression<EqualsToken>
        | [<CompiledValue 224>] SpreadElement of SpreadElement
        | [<CompiledValue 226>] OmittedExpression of OmittedExpression

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] BindingOrAssignmentElementRestIndicator =
        | [<CompiledValue 25>] DotDotDotToken of DotDotDotToken
        | [<CompiledValue 224>] SpreadElement of SpreadElement
        | [<CompiledValue 296>] SpreadAssignment of SpreadAssignment

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] BindingOrAssignmentElementTarget =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 200>] ObjectBindingPattern of ObjectBindingPattern
        | [<CompiledValue 201>] ArrayBindingPattern of ArrayBindingPattern
        | [<CompiledValue 203>] ArrayLiteralExpression of ArrayLiteralExpression
        | [<CompiledValue 204>] ObjectLiteralExpression of ObjectLiteralExpression
        | [<CompiledValue 205>] PropertyAccessExpression of PropertyAccessExpression
        | [<CompiledValue 206>] ElementAccessExpression of ElementAccessExpression
        | [<CompiledValue 226>] OmittedExpression of OmittedExpression

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ObjectBindingOrAssignmentPattern =
        | [<CompiledValue 200>] ObjectBindingPattern of ObjectBindingPattern
        | [<CompiledValue 204>] ObjectLiteralExpression of ObjectLiteralExpression

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ArrayBindingOrAssignmentPattern =
        | [<CompiledValue 201>] ArrayBindingPattern of ArrayBindingPattern
        | [<CompiledValue 203>] ArrayLiteralExpression of ArrayLiteralExpression

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] AssignmentPattern =
        | [<CompiledValue 203>] ArrayLiteralExpression of ArrayLiteralExpression
        | [<CompiledValue 204>] ObjectLiteralExpression of ObjectLiteralExpression

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] BindingOrAssignmentPattern =
        | [<CompiledValue 200>] ObjectBindingPattern of ObjectBindingPattern
        | [<CompiledValue 201>] ArrayBindingPattern of ArrayBindingPattern
        | [<CompiledValue 203>] ArrayLiteralExpression of ArrayLiteralExpression
        | [<CompiledValue 204>] ObjectLiteralExpression of ObjectLiteralExpression

    type [<AllowNullLiteral>] ConditionalExpression =
        inherit Expression
        abstract kind: SyntaxKind
        abstract condition: Expression
        abstract questionToken: QuestionToken
        abstract whenTrue: Expression
        abstract colonToken: ColonToken
        abstract whenFalse: Expression

    type FunctionBody =
        Block

    type ConciseBody =
        U2<FunctionBody, Expression>

    type [<AllowNullLiteral>] FunctionExpression =
        inherit PrimaryExpression
        inherit FunctionLikeDeclarationBase
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract name: Identifier option
        abstract body: FunctionBody

    type [<AllowNullLiteral>] ArrowFunction =
        inherit Expression
        inherit FunctionLikeDeclarationBase
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract equalsGreaterThanToken: EqualsGreaterThanToken
        abstract body: ConciseBody
        abstract name: obj

    type [<AllowNullLiteral>] LiteralLikeNode =
        inherit Node
        abstract text: string with get, set
        abstract isUnterminated: bool option with get, set
        abstract hasExtendedUnicodeEscape: bool option with get, set

    type [<AllowNullLiteral>] TemplateLiteralLikeNode =
        inherit LiteralLikeNode
        abstract rawText: string option with get, set

    type [<AllowNullLiteral>] LiteralExpression =
        inherit LiteralLikeNode
        inherit PrimaryExpression
        abstract _literalExpressionBrand: obj option with get, set

    type [<AllowNullLiteral>] RegularExpressionLiteral =
        inherit LiteralExpression
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] NoSubstitutionTemplateLiteral =
        inherit LiteralExpression
        inherit TemplateLiteralLikeNode
        inherit Declaration
        abstract kind: SyntaxKind

    type [<RequireQualifiedAccess>] TokenFlags =
        | None = 0
        | Scientific = 16
        | Octal = 32
        | HexSpecifier = 64
        | BinarySpecifier = 128
        | OctalSpecifier = 256

    type [<AllowNullLiteral>] NumericLiteral =
        inherit LiteralExpression
        inherit Declaration
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] BigIntLiteral =
        inherit LiteralExpression
        abstract kind: SyntaxKind

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] LiteralToken =
        | [<CompiledValue 8>] NumericLiteral of NumericLiteral
        | [<CompiledValue 9>] BigIntLiteral of BigIntLiteral
        | [<CompiledValue 10>] StringLiteral of StringLiteral
        | [<CompiledValue 11>] JsxText of JsxText
        | [<CompiledValue 13>] RegularExpressionLiteral of RegularExpressionLiteral
        | [<CompiledValue 14>] NoSubstitutionTemplateLiteral of NoSubstitutionTemplateLiteral

    type [<AllowNullLiteral>] TemplateHead =
        inherit TemplateLiteralLikeNode
        abstract kind: SyntaxKind
        abstract parent: U2<TemplateExpression, TemplateLiteralTypeNode>

    type [<AllowNullLiteral>] TemplateMiddle =
        inherit TemplateLiteralLikeNode
        abstract kind: SyntaxKind
        abstract parent: U2<TemplateSpan, TemplateLiteralTypeSpan>

    type [<AllowNullLiteral>] TemplateTail =
        inherit TemplateLiteralLikeNode
        abstract kind: SyntaxKind
        abstract parent: U2<TemplateSpan, TemplateLiteralTypeSpan>

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] PseudoLiteralToken =
        | [<CompiledValue 15>] TemplateHead of TemplateHead
        | [<CompiledValue 16>] TemplateMiddle of TemplateMiddle
        | [<CompiledValue 17>] TemplateTail of TemplateTail

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] TemplateLiteralToken =
        | [<CompiledValue 14>] NoSubstitutionTemplateLiteral of NoSubstitutionTemplateLiteral
        | [<CompiledValue 15>] TemplateHead of TemplateHead
        | [<CompiledValue 16>] TemplateMiddle of TemplateMiddle
        | [<CompiledValue 17>] TemplateTail of TemplateTail

    type [<AllowNullLiteral>] TemplateExpression =
        inherit PrimaryExpression
        abstract kind: SyntaxKind
        abstract head: TemplateHead
        abstract templateSpans: TemplateSpan[]

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] TemplateLiteral =
        | [<CompiledValue 14>] NoSubstitutionTemplateLiteral of NoSubstitutionTemplateLiteral
        | [<CompiledValue 222>] TemplateExpression of TemplateExpression

    type [<AllowNullLiteral>] TemplateSpan =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: TemplateExpression
        abstract expression: Expression
        abstract literal: U2<TemplateMiddle, TemplateTail>

    type [<AllowNullLiteral>] ParenthesizedExpression =
        inherit PrimaryExpression
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract expression: Expression

    type [<AllowNullLiteral>] ArrayLiteralExpression =
        inherit PrimaryExpression
        abstract kind: SyntaxKind
        abstract elements: Expression[]

    type [<AllowNullLiteral>] SpreadElement =
        inherit Expression
        abstract kind: SyntaxKind
        abstract parent: U3<ArrayLiteralExpression, CallExpression, NewExpression>
        abstract expression: Expression

    /// This interface is a base interface for ObjectLiteralExpression and JSXAttributes to extend from. JSXAttributes is similar to
    /// ObjectLiteralExpression in that it contains array of properties; however, JSXAttributes' properties can only be
    /// JSXAttribute or JSXSpreadAttribute. ObjectLiteralExpression, on the other hand, can only have properties of type
    /// ObjectLiteralElement (e.g. PropertyAssignment, ShorthandPropertyAssignment etc.)
    type [<AllowNullLiteral>] ObjectLiteralExpressionBase<'T> =
        inherit PrimaryExpression
        inherit Declaration
        abstract properties: 'T[]

    type [<AllowNullLiteral>] ObjectLiteralExpression =
        inherit ObjectLiteralExpressionBase<ObjectLiteralElementLike>
        abstract kind: SyntaxKind

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] EntityNameExpression =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 205>] PropertyAccessEntityNameExpression of PropertyAccessEntityNameExpression

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] EntityNameOrEntityNameExpression =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 160>] QualifiedName of QualifiedName
        | [<CompiledValue 205>] PropertyAccessEntityNameExpression of PropertyAccessEntityNameExpression

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] AccessExpression =
        | [<CompiledValue 205>] PropertyAccessExpression of PropertyAccessExpression
        | [<CompiledValue 206>] ElementAccessExpression of ElementAccessExpression

    type [<AllowNullLiteral>] PropertyAccessExpression =
        inherit MemberExpression
        inherit NamedDeclaration
        abstract kind: SyntaxKind
        abstract expression: LeftHandSideExpression
        abstract questionDotToken: QuestionDotToken option
        abstract name: MemberName

    type [<AllowNullLiteral>] PropertyAccessChain =
        inherit PropertyAccessExpression
        abstract _optionalChainBrand: obj option with get, set
        abstract name: MemberName

    type [<AllowNullLiteral>] SuperPropertyAccessExpression =
        inherit PropertyAccessExpression
        abstract expression: SuperExpression

    /// Brand for a PropertyAccessExpression which, like a QualifiedName, consists of a sequence of identifiers separated by dots.
    type [<AllowNullLiteral>] PropertyAccessEntityNameExpression =
        inherit PropertyAccessExpression
        abstract _propertyAccessExpressionLikeQualifiedNameBrand: obj option with get, set
        abstract expression: EntityNameExpression
        abstract name: Identifier

    type [<AllowNullLiteral>] ElementAccessExpression =
        inherit MemberExpression
        abstract kind: SyntaxKind
        abstract expression: LeftHandSideExpression
        abstract questionDotToken: QuestionDotToken option
        abstract argumentExpression: Expression

    type [<AllowNullLiteral>] ElementAccessChain =
        inherit ElementAccessExpression
        abstract _optionalChainBrand: obj option with get, set

    type [<AllowNullLiteral>] SuperElementAccessExpression =
        inherit ElementAccessExpression
        abstract expression: SuperExpression

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] SuperProperty =
        | [<CompiledValue 205>] SuperPropertyAccessExpression of SuperPropertyAccessExpression
        | [<CompiledValue 206>] SuperElementAccessExpression of SuperElementAccessExpression

    type [<AllowNullLiteral>] CallExpression =
        inherit LeftHandSideExpression
        inherit Declaration
        abstract kind: SyntaxKind
        abstract expression: LeftHandSideExpression
        abstract questionDotToken: QuestionDotToken option
        abstract typeArguments: TypeNode[] option
        abstract arguments: Expression[]

    type [<AllowNullLiteral>] CallChain =
        inherit CallExpression
        abstract _optionalChainBrand: obj option with get, set

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] OptionalChain =
        | [<CompiledValue 205>] PropertyAccessChain of PropertyAccessChain
        | [<CompiledValue 206>] ElementAccessChain of ElementAccessChain
        | [<CompiledValue 207>] CallChain of CallChain
        | [<CompiledValue 229>] NonNullChain of NonNullChain

    type [<AllowNullLiteral>] SuperCall =
        inherit CallExpression
        abstract expression: SuperExpression

    type [<AllowNullLiteral>] ImportCall =
        inherit CallExpression
        abstract expression: ImportExpression

    type [<AllowNullLiteral>] ExpressionWithTypeArguments =
        inherit NodeWithTypeArguments
        abstract kind: SyntaxKind
        abstract parent: U3<HeritageClause, JSDocAugmentsTag, JSDocImplementsTag>
        abstract expression: LeftHandSideExpression

    type [<AllowNullLiteral>] NewExpression =
        inherit PrimaryExpression
        inherit Declaration
        abstract kind: SyntaxKind
        abstract expression: LeftHandSideExpression
        abstract typeArguments: TypeNode[] option
        abstract arguments: Expression[] option

    type [<AllowNullLiteral>] TaggedTemplateExpression =
        inherit MemberExpression
        abstract kind: SyntaxKind
        abstract tag: LeftHandSideExpression
        abstract typeArguments: TypeNode[] option
        abstract template: TemplateLiteral

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] CallLikeExpression =
        | [<CompiledValue 164>] Decorator of Decorator
        | [<CompiledValue 207>] CallExpression of CallExpression
        | [<CompiledValue 208>] NewExpression of NewExpression
        | [<CompiledValue 209>] TaggedTemplateExpression of TaggedTemplateExpression
        | [<CompiledValue 278>] JsxSelfClosingElement of JsxSelfClosingElement
        | [<CompiledValue 279>] JsxOpeningElement of JsxOpeningElement

    type [<AllowNullLiteral>] AsExpression =
        inherit Expression
        abstract kind: SyntaxKind
        abstract expression: Expression
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] TypeAssertion =
        inherit UnaryExpression
        abstract kind: SyntaxKind
        abstract ``type``: TypeNode
        abstract expression: UnaryExpression

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] AssertionExpression =
        | [<CompiledValue 210>] TypeAssertion of TypeAssertion
        | [<CompiledValue 228>] AsExpression of AsExpression

    type [<AllowNullLiteral>] NonNullExpression =
        inherit LeftHandSideExpression
        abstract kind: SyntaxKind
        abstract expression: Expression

    type [<AllowNullLiteral>] NonNullChain =
        inherit NonNullExpression
        abstract _optionalChainBrand: obj option with get, set

    type [<AllowNullLiteral>] MetaProperty =
        inherit PrimaryExpression
        abstract kind: SyntaxKind
        abstract keywordToken: SyntaxKind
        abstract name: Identifier

    type [<AllowNullLiteral>] JsxElement =
        inherit PrimaryExpression
        abstract kind: SyntaxKind
        abstract openingElement: JsxOpeningElement
        abstract children: JsxChild[]
        abstract closingElement: JsxClosingElement

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] JsxOpeningLikeElement =
        | [<CompiledValue 278>] JsxSelfClosingElement of JsxSelfClosingElement
        | [<CompiledValue 279>] JsxOpeningElement of JsxOpeningElement

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] JsxAttributeLike =
        | [<CompiledValue 284>] JsxAttribute of JsxAttribute
        | [<CompiledValue 286>] JsxSpreadAttribute of JsxSpreadAttribute

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] JsxTagNameExpression =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 108>] ThisExpression of ThisExpression
        | [<CompiledValue 205>] JsxTagNamePropertyAccess of JsxTagNamePropertyAccess

    type [<AllowNullLiteral>] JsxTagNamePropertyAccess =
        inherit PropertyAccessExpression
        abstract expression: JsxTagNameExpression

    type [<AllowNullLiteral>] JsxAttributes =
        inherit ObjectLiteralExpressionBase<JsxAttributeLike>
        abstract kind: SyntaxKind
        abstract parent: JsxOpeningLikeElement

    type [<AllowNullLiteral>] JsxOpeningElement =
        inherit Expression
        abstract kind: SyntaxKind
        abstract parent: JsxElement
        abstract tagName: JsxTagNameExpression
        abstract typeArguments: TypeNode[] option
        abstract attributes: JsxAttributes

    type [<AllowNullLiteral>] JsxSelfClosingElement =
        inherit PrimaryExpression
        abstract kind: SyntaxKind
        abstract tagName: JsxTagNameExpression
        abstract typeArguments: TypeNode[] option
        abstract attributes: JsxAttributes

    type [<AllowNullLiteral>] JsxFragment =
        inherit PrimaryExpression
        abstract kind: SyntaxKind
        abstract openingFragment: JsxOpeningFragment
        abstract children: JsxChild[]
        abstract closingFragment: JsxClosingFragment

    type [<AllowNullLiteral>] JsxOpeningFragment =
        inherit Expression
        abstract kind: SyntaxKind
        abstract parent: JsxFragment

    type [<AllowNullLiteral>] JsxClosingFragment =
        inherit Expression
        abstract kind: SyntaxKind
        abstract parent: JsxFragment

    type [<AllowNullLiteral>] JsxAttribute =
        inherit ObjectLiteralElement
        abstract kind: SyntaxKind
        abstract parent: JsxAttributes
        abstract name: Identifier
        abstract initializer: U2<StringLiteral, JsxExpression> option

    type [<AllowNullLiteral>] JsxSpreadAttribute =
        inherit ObjectLiteralElement
        abstract kind: SyntaxKind
        abstract parent: JsxAttributes
        abstract expression: Expression

    type [<AllowNullLiteral>] JsxClosingElement =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: JsxElement
        abstract tagName: JsxTagNameExpression

    type [<AllowNullLiteral>] JsxExpression =
        inherit Expression
        abstract kind: SyntaxKind
        abstract parent: U3<JsxElement, JsxFragment, JsxAttributeLike>
        abstract dotDotDotToken: Token<SyntaxKind> option
        abstract expression: Expression option

    type [<AllowNullLiteral>] JsxText =
        inherit LiteralLikeNode
        abstract kind: SyntaxKind
        abstract parent: U2<JsxElement, JsxFragment>
        abstract containsOnlyTriviaWhiteSpaces: bool

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] JsxChild =
        | [<CompiledValue 11>] JsxText of JsxText
        | [<CompiledValue 277>] JsxElement of JsxElement
        | [<CompiledValue 278>] JsxSelfClosingElement of JsxSelfClosingElement
        | [<CompiledValue 281>] JsxFragment of JsxFragment
        | [<CompiledValue 287>] JsxExpression of JsxExpression

    type [<AllowNullLiteral>] Statement =
        inherit Node
        inherit JSDocContainer
        abstract _statementBrand: obj option with get, set

    type [<AllowNullLiteral>] NotEmittedStatement =
        inherit Statement
        abstract kind: SyntaxKind

    /// A list of comma-separated expressions. This node is only created by transformations.
    type [<AllowNullLiteral>] CommaListExpression =
        inherit Expression
        abstract kind: SyntaxKind
        abstract elements: Expression[]

    type [<AllowNullLiteral>] EmptyStatement =
        inherit Statement
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] DebuggerStatement =
        inherit Statement
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] MissingDeclaration =
        inherit DeclarationStatement
        abstract kind: SyntaxKind
        abstract name: Identifier option

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] BlockLike =
        | [<CompiledValue 234>] Block of Block
        | [<CompiledValue 261>] ModuleBlock of ModuleBlock
        | [<CompiledValue 288>] CaseClause of CaseClause
        | [<CompiledValue 289>] DefaultClause of DefaultClause
        | [<CompiledValue 303>] SourceFile of SourceFile

    type [<AllowNullLiteral>] Block =
        inherit Statement
        abstract kind: SyntaxKind
        abstract statements: Statement[]

    type [<AllowNullLiteral>] VariableStatement =
        inherit Statement
        abstract kind: SyntaxKind
        abstract declarationList: VariableDeclarationList

    type [<AllowNullLiteral>] ExpressionStatement =
        inherit Statement
        abstract kind: SyntaxKind
        abstract expression: Expression

    type [<AllowNullLiteral>] IfStatement =
        inherit Statement
        abstract kind: SyntaxKind
        abstract expression: Expression
        abstract thenStatement: Statement
        abstract elseStatement: Statement option

    type [<AllowNullLiteral>] IterationStatement =
        inherit Statement
        abstract statement: Statement

    type [<AllowNullLiteral>] DoStatement =
        inherit IterationStatement
        abstract kind: SyntaxKind
        abstract expression: Expression

    type [<AllowNullLiteral>] WhileStatement =
        inherit IterationStatement
        abstract kind: SyntaxKind
        abstract expression: Expression

    type ForInitializer =
        U2<VariableDeclarationList, Expression>

    type [<AllowNullLiteral>] ForStatement =
        inherit IterationStatement
        abstract kind: SyntaxKind
        abstract initializer: ForInitializer option
        abstract condition: Expression option
        abstract incrementor: Expression option

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ForInOrOfStatement =
        | [<CompiledValue 242>] ForInStatement of ForInStatement
        | [<CompiledValue 243>] ForOfStatement of ForOfStatement

    type [<AllowNullLiteral>] ForInStatement =
        inherit IterationStatement
        abstract kind: SyntaxKind
        abstract initializer: ForInitializer
        abstract expression: Expression

    type [<AllowNullLiteral>] ForOfStatement =
        inherit IterationStatement
        abstract kind: SyntaxKind
        abstract awaitModifier: AwaitKeyword option
        abstract initializer: ForInitializer
        abstract expression: Expression

    type [<AllowNullLiteral>] BreakStatement =
        inherit Statement
        abstract kind: SyntaxKind
        abstract label: Identifier option

    type [<AllowNullLiteral>] ContinueStatement =
        inherit Statement
        abstract kind: SyntaxKind
        abstract label: Identifier option

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] BreakOrContinueStatement =
        | [<CompiledValue 244>] ContinueStatement of ContinueStatement
        | [<CompiledValue 245>] BreakStatement of BreakStatement

    type [<AllowNullLiteral>] ReturnStatement =
        inherit Statement
        abstract kind: SyntaxKind
        abstract expression: Expression option

    type [<AllowNullLiteral>] WithStatement =
        inherit Statement
        abstract kind: SyntaxKind
        abstract expression: Expression
        abstract statement: Statement

    type [<AllowNullLiteral>] SwitchStatement =
        inherit Statement
        abstract kind: SyntaxKind
        abstract expression: Expression
        abstract caseBlock: CaseBlock
        abstract possiblyExhaustive: bool option with get, set

    type [<AllowNullLiteral>] CaseBlock =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: SwitchStatement
        abstract clauses: CaseOrDefaultClause[]

    type [<AllowNullLiteral>] CaseClause =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: CaseBlock
        abstract expression: Expression
        abstract statements: Statement[]

    type [<AllowNullLiteral>] DefaultClause =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: CaseBlock
        abstract statements: Statement[]

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] CaseOrDefaultClause =
        | [<CompiledValue 288>] CaseClause of CaseClause
        | [<CompiledValue 289>] DefaultClause of DefaultClause

    type [<AllowNullLiteral>] LabeledStatement =
        inherit Statement
        abstract kind: SyntaxKind
        abstract label: Identifier
        abstract statement: Statement

    type [<AllowNullLiteral>] ThrowStatement =
        inherit Statement
        abstract kind: SyntaxKind
        abstract expression: Expression

    type [<AllowNullLiteral>] TryStatement =
        inherit Statement
        abstract kind: SyntaxKind
        abstract tryBlock: Block
        abstract catchClause: CatchClause option
        abstract finallyBlock: Block option

    type [<AllowNullLiteral>] CatchClause =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: TryStatement
        abstract variableDeclaration: VariableDeclaration option
        abstract block: Block

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ObjectTypeDeclaration =
        | [<CompiledValue 181>] TypeLiteralNode of TypeLiteralNode
        | [<CompiledValue 225>] ClassExpression of ClassExpression
        | [<CompiledValue 256>] ClassDeclaration of ClassDeclaration
        | [<CompiledValue 257>] InterfaceDeclaration of InterfaceDeclaration

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] DeclarationWithTypeParameters =
        | [<CompiledValue 167>] MethodSignature of MethodSignature
        | [<CompiledValue 168>] MethodDeclaration of MethodDeclaration
        | [<CompiledValue 170>] ConstructorDeclaration of ConstructorDeclaration
        | [<CompiledValue 171>] GetAccessorDeclaration of GetAccessorDeclaration
        | [<CompiledValue 172>] SetAccessorDeclaration of SetAccessorDeclaration
        | [<CompiledValue 173>] CallSignatureDeclaration of CallSignatureDeclaration
        | [<CompiledValue 174>] ConstructSignatureDeclaration of ConstructSignatureDeclaration
        | [<CompiledValue 175>] IndexSignatureDeclaration of IndexSignatureDeclaration
        | [<CompiledValue 178>] FunctionTypeNode of FunctionTypeNode
        | [<CompiledValue 179>] ConstructorTypeNode of ConstructorTypeNode
        | [<CompiledValue 212>] FunctionExpression of FunctionExpression
        | [<CompiledValue 213>] ArrowFunction of ArrowFunction
        | [<CompiledValue 225>] ClassExpression of ClassExpression
        | [<CompiledValue 255>] FunctionDeclaration of FunctionDeclaration
        | [<CompiledValue 256>] ClassDeclaration of ClassDeclaration
        | [<CompiledValue 257>] InterfaceDeclaration of InterfaceDeclaration
        | [<CompiledValue 258>] TypeAliasDeclaration of TypeAliasDeclaration
        | [<CompiledValue 315>] JSDocFunctionType of JSDocFunctionType
        | [<CompiledValue 321>] JSDocSignature of JSDocSignature
        | [<CompiledValue 336>] JSDocCallbackTag of JSDocCallbackTag
        | [<CompiledValue 342>] JSDocTemplateTag of JSDocTemplateTag
        | [<CompiledValue 343>] JSDocTypedefTag of JSDocTypedefTag

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] DeclarationWithTypeParameterChildren =
        | [<CompiledValue 167>] MethodSignature of MethodSignature
        | [<CompiledValue 168>] MethodDeclaration of MethodDeclaration
        | [<CompiledValue 170>] ConstructorDeclaration of ConstructorDeclaration
        | [<CompiledValue 171>] GetAccessorDeclaration of GetAccessorDeclaration
        | [<CompiledValue 172>] SetAccessorDeclaration of SetAccessorDeclaration
        | [<CompiledValue 173>] CallSignatureDeclaration of CallSignatureDeclaration
        | [<CompiledValue 174>] ConstructSignatureDeclaration of ConstructSignatureDeclaration
        | [<CompiledValue 175>] IndexSignatureDeclaration of IndexSignatureDeclaration
        | [<CompiledValue 178>] FunctionTypeNode of FunctionTypeNode
        | [<CompiledValue 179>] ConstructorTypeNode of ConstructorTypeNode
        | [<CompiledValue 212>] FunctionExpression of FunctionExpression
        | [<CompiledValue 213>] ArrowFunction of ArrowFunction
        | [<CompiledValue 225>] ClassExpression of ClassExpression
        | [<CompiledValue 255>] FunctionDeclaration of FunctionDeclaration
        | [<CompiledValue 256>] ClassDeclaration of ClassDeclaration
        | [<CompiledValue 257>] InterfaceDeclaration of InterfaceDeclaration
        | [<CompiledValue 258>] TypeAliasDeclaration of TypeAliasDeclaration
        | [<CompiledValue 315>] JSDocFunctionType of JSDocFunctionType
        | [<CompiledValue 342>] JSDocTemplateTag of JSDocTemplateTag

    type [<AllowNullLiteral>] ClassLikeDeclarationBase =
        inherit NamedDeclaration
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract name: Identifier option
        abstract typeParameters: TypeParameterDeclaration[] option
        abstract heritageClauses: HeritageClause[] option
        abstract members: ClassElement[]

    type [<AllowNullLiteral>] ClassDeclaration =
        inherit ClassLikeDeclarationBase
        inherit DeclarationStatement
        abstract kind: SyntaxKind
        /// <summary>May be undefined in <c>export default class { ... }</c>.</summary>
        abstract name: Identifier option

    type [<AllowNullLiteral>] ClassExpression =
        inherit ClassLikeDeclarationBase
        inherit PrimaryExpression
        abstract kind: SyntaxKind

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ClassLikeDeclaration =
        | [<CompiledValue 225>] ClassExpression of ClassExpression
        | [<CompiledValue 256>] ClassDeclaration of ClassDeclaration

    type [<AllowNullLiteral>] ClassElement =
        inherit NamedDeclaration
        abstract _classElementBrand: obj option with get, set
        abstract name: PropertyName option

    type [<AllowNullLiteral>] TypeElement =
        inherit NamedDeclaration
        abstract _typeElementBrand: obj option with get, set
        abstract name: PropertyName option
        abstract questionToken: QuestionToken option

    type [<AllowNullLiteral>] InterfaceDeclaration =
        inherit DeclarationStatement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract name: Identifier
        abstract typeParameters: TypeParameterDeclaration[] option
        abstract heritageClauses: HeritageClause[] option
        abstract members: TypeElement[]

    type [<AllowNullLiteral>] HeritageClause =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: U2<InterfaceDeclaration, ClassLikeDeclaration>
        abstract token: SyntaxKind
        abstract types: ExpressionWithTypeArguments[]

    type [<AllowNullLiteral>] TypeAliasDeclaration =
        inherit DeclarationStatement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract name: Identifier
        abstract typeParameters: TypeParameterDeclaration[] option
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] EnumMember =
        inherit NamedDeclaration
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: EnumDeclaration
        abstract name: PropertyName
        abstract initializer: Expression option

    type [<AllowNullLiteral>] EnumDeclaration =
        inherit DeclarationStatement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract name: Identifier
        abstract members: EnumMember[]

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ModuleName =
        | [<CompiledValue 10>] StringLiteral of StringLiteral
        | [<CompiledValue 79>] Identifier of Identifier

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ModuleBody =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 260>] Empty of U2<NamespaceDeclaration, JSDocNamespaceDeclaration>
        | [<CompiledValue 261>] ModuleBlock of ModuleBlock

    type [<AllowNullLiteral>] ModuleDeclaration =
        inherit DeclarationStatement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: U2<ModuleBody, SourceFile>
        abstract name: ModuleName
        abstract body: U2<ModuleBody, JSDocNamespaceDeclaration> option

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] NamespaceBody =
        | [<CompiledValue 260>] NamespaceDeclaration of NamespaceDeclaration
        | [<CompiledValue 261>] ModuleBlock of ModuleBlock

    type [<AllowNullLiteral>] NamespaceDeclaration =
        inherit ModuleDeclaration
        abstract name: Identifier
        abstract body: NamespaceBody

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] JSDocNamespaceBody =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 260>] JSDocNamespaceDeclaration of JSDocNamespaceDeclaration

    type [<AllowNullLiteral>] JSDocNamespaceDeclaration =
        inherit ModuleDeclaration
        abstract name: Identifier
        abstract body: JSDocNamespaceBody option

    type [<AllowNullLiteral>] ModuleBlock =
        inherit Node
        inherit Statement
        abstract kind: SyntaxKind
        abstract parent: ModuleDeclaration
        abstract statements: Statement[]

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ModuleReference =
        | [<CompiledValue 79>] Identifier of Identifier
        | [<CompiledValue 160>] QualifiedName of QualifiedName
        | [<CompiledValue 276>] ExternalModuleReference of ExternalModuleReference

    /// One of:
    /// - import x = require("mod");
    /// - import x = M.x;
    type [<AllowNullLiteral>] ImportEqualsDeclaration =
        inherit DeclarationStatement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: U2<SourceFile, ModuleBlock>
        abstract name: Identifier
        abstract isTypeOnly: bool
        abstract moduleReference: ModuleReference

    type [<AllowNullLiteral>] ExternalModuleReference =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: ImportEqualsDeclaration
        abstract expression: Expression

    type [<AllowNullLiteral>] ImportDeclaration =
        inherit Statement
        abstract kind: SyntaxKind
        abstract parent: U2<SourceFile, ModuleBlock>
        abstract importClause: ImportClause option
        /// If this is not a StringLiteral it will be a grammar error.
        abstract moduleSpecifier: Expression
        abstract assertClause: AssertClause option

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] NamedImportBindings =
        | [<CompiledValue 267>] NamespaceImport of NamespaceImport
        | [<CompiledValue 268>] NamedImports of NamedImports

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] NamedExportBindings =
        | [<CompiledValue 272>] NamedExports of NamedExports
        | [<CompiledValue 273>] NamespaceExport of NamespaceExport

    type [<AllowNullLiteral>] ImportClause =
        inherit NamedDeclaration
        abstract kind: SyntaxKind
        abstract parent: ImportDeclaration
        abstract isTypeOnly: bool
        abstract name: Identifier option
        abstract namedBindings: NamedImportBindings option

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] AssertionKey =
        | [<CompiledValue 10>] StringLiteral of StringLiteral
        | [<CompiledValue 79>] Identifier of Identifier

    type [<AllowNullLiteral>] AssertEntry =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: AssertClause
        abstract name: AssertionKey
        abstract value: Expression

    type [<AllowNullLiteral>] AssertClause =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: U2<ImportDeclaration, ExportDeclaration>
        abstract elements: AssertEntry[]
        abstract multiLine: bool option

    type [<AllowNullLiteral>] NamespaceImport =
        inherit NamedDeclaration
        abstract kind: SyntaxKind
        abstract parent: ImportClause
        abstract name: Identifier

    type [<AllowNullLiteral>] NamespaceExport =
        inherit NamedDeclaration
        abstract kind: SyntaxKind
        abstract parent: ExportDeclaration
        abstract name: Identifier

    type [<AllowNullLiteral>] NamespaceExportDeclaration =
        inherit DeclarationStatement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract name: Identifier

    type [<AllowNullLiteral>] ExportDeclaration =
        inherit DeclarationStatement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: U2<SourceFile, ModuleBlock>
        abstract isTypeOnly: bool
        /// <summary>Will not be assigned in the case of <c>export * from "foo";</c></summary>
        abstract exportClause: NamedExportBindings option
        /// If this is not a StringLiteral it will be a grammar error.
        abstract moduleSpecifier: Expression option
        abstract assertClause: AssertClause option

    type [<AllowNullLiteral>] NamedImports =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: ImportClause
        abstract elements: ImportSpecifier[]

    type [<AllowNullLiteral>] NamedExports =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: ExportDeclaration
        abstract elements: ExportSpecifier[]

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] NamedImportsOrExports =
        | [<CompiledValue 268>] NamedImports of NamedImports
        | [<CompiledValue 272>] NamedExports of NamedExports

    type [<AllowNullLiteral>] ImportSpecifier =
        inherit NamedDeclaration
        abstract kind: SyntaxKind
        abstract parent: NamedImports
        abstract propertyName: Identifier option
        abstract name: Identifier
        abstract isTypeOnly: bool

    type [<AllowNullLiteral>] ExportSpecifier =
        inherit NamedDeclaration
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: NamedExports
        abstract isTypeOnly: bool
        abstract propertyName: Identifier option
        abstract name: Identifier

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] ImportOrExportSpecifier =
        | [<CompiledValue 269>] ImportSpecifier of ImportSpecifier
        | [<CompiledValue 274>] ExportSpecifier of ExportSpecifier

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] TypeOnlyCompatibleAliasDeclaration =
        | [<CompiledValue 264>] ImportEqualsDeclaration of ImportEqualsDeclaration
        | [<CompiledValue 266>] ImportClause of ImportClause
        | [<CompiledValue 267>] NamespaceImport of NamespaceImport
        | [<CompiledValue 269>] ImportSpecifier of ImportSpecifier
        | [<CompiledValue 274>] ExportSpecifier of ExportSpecifier

    type TypeOnlyAliasDeclaration =
        obj

    /// <summary>
    /// This is either an <c>export =</c> or an <c>export default</c> declaration.
    /// Unless <c>isExportEquals</c> is set, this node was parsed as an <c>export default</c>.
    /// </summary>
    type [<AllowNullLiteral>] ExportAssignment =
        inherit DeclarationStatement
        inherit JSDocContainer
        abstract kind: SyntaxKind
        abstract parent: SourceFile
        abstract isExportEquals: bool option
        abstract expression: Expression

    type [<AllowNullLiteral>] FileReference =
        inherit TextRange
        abstract fileName: string with get, set

    type [<AllowNullLiteral>] CheckJsDirective =
        inherit TextRange
        abstract enabled: bool with get, set

    type CommentKind =
        SyntaxKind

    type [<AllowNullLiteral>] CommentRange =
        inherit TextRange
        abstract hasTrailingNewLine: bool option with get, set
        abstract kind: CommentKind with get, set

    type [<AllowNullLiteral>] SynthesizedComment =
        inherit CommentRange
        abstract text: string with get, set
        abstract pos: int with get, set
        abstract ``end``: int with get, set
        abstract hasLeadingNewline: bool option with get, set

    type [<AllowNullLiteral>] JSDocTypeExpression =
        inherit TypeNode
        abstract kind: SyntaxKind
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] JSDocNameReference =
        inherit Node
        abstract kind: SyntaxKind
        abstract name: U2<EntityName, JSDocMemberName>

    /// Class#method reference in JSDoc
    type [<AllowNullLiteral>] JSDocMemberName =
        inherit Node
        abstract kind: SyntaxKind
        abstract left: U2<EntityName, JSDocMemberName>
        abstract right: Identifier

    type [<AllowNullLiteral>] JSDocType =
        inherit TypeNode
        abstract _jsDocTypeBrand: obj option with get, set

    type [<AllowNullLiteral>] JSDocAllType =
        inherit JSDocType
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocUnknownType =
        inherit JSDocType
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocNonNullableType =
        inherit JSDocType
        abstract kind: SyntaxKind
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] JSDocNullableType =
        inherit JSDocType
        abstract kind: SyntaxKind
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] JSDocOptionalType =
        inherit JSDocType
        abstract kind: SyntaxKind
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] JSDocFunctionType =
        inherit JSDocType
        inherit SignatureDeclarationBase
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocVariadicType =
        inherit JSDocType
        abstract kind: SyntaxKind
        abstract ``type``: TypeNode

    type [<AllowNullLiteral>] JSDocNamepathType =
        inherit JSDocType
        abstract kind: SyntaxKind
        abstract ``type``: TypeNode

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] JSDocTypeReferencingNode =
        | [<CompiledValue 312>] JSDocNullableType of JSDocNullableType
        | [<CompiledValue 313>] JSDocNonNullableType of JSDocNonNullableType
        | [<CompiledValue 314>] JSDocOptionalType of JSDocOptionalType
        | [<CompiledValue 316>] JSDocVariadicType of JSDocVariadicType

    type [<AllowNullLiteral>] JSDoc =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: HasJSDoc
        abstract tags: JSDocTag[] option
        abstract comment: U2<string, JSDocComment[]> option

    type [<AllowNullLiteral>] JSDocTag =
        inherit Node
        abstract parent: U2<JSDoc, JSDocTypeLiteral>
        abstract tagName: Identifier
        abstract comment: U2<string, JSDocComment[]> option

    type [<AllowNullLiteral>] JSDocLink =
        inherit Node
        abstract kind: SyntaxKind
        abstract name: U2<EntityName, JSDocMemberName> option
        abstract text: string with get, set

    type [<AllowNullLiteral>] JSDocLinkCode =
        inherit Node
        abstract kind: SyntaxKind
        abstract name: U2<EntityName, JSDocMemberName> option
        abstract text: string with get, set

    type [<AllowNullLiteral>] JSDocLinkPlain =
        inherit Node
        abstract kind: SyntaxKind
        abstract name: U2<EntityName, JSDocMemberName> option
        abstract text: string with get, set

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] JSDocComment =
        | [<CompiledValue 319>] JSDocText of JSDocText
        | [<CompiledValue 322>] JSDocLink of JSDocLink
        | [<CompiledValue 323>] JSDocLinkCode of JSDocLinkCode
        | [<CompiledValue 324>] JSDocLinkPlain of JSDocLinkPlain

    type [<AllowNullLiteral>] JSDocText =
        inherit Node
        abstract kind: SyntaxKind
        abstract text: string with get, set

    type [<AllowNullLiteral>] JSDocUnknownTag =
        inherit JSDocTag
        abstract kind: SyntaxKind

    /// <summary>
    /// Note that <c>@extends</c> is a synonym of <c>@augments</c>.
    /// Both tags are represented by this interface.
    /// </summary>
    type [<AllowNullLiteral>] JSDocAugmentsTag =
        inherit JSDocTag
        abstract kind: SyntaxKind
        abstract ``class``: obj

    type [<AllowNullLiteral>] JSDocImplementsTag =
        inherit JSDocTag
        abstract kind: SyntaxKind
        abstract ``class``: obj

    type [<AllowNullLiteral>] JSDocAuthorTag =
        inherit JSDocTag
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocDeprecatedTag =
        inherit JSDocTag
        abstract kind: SyntaxKind with get, set

    type [<AllowNullLiteral>] JSDocClassTag =
        inherit JSDocTag
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocPublicTag =
        inherit JSDocTag
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocPrivateTag =
        inherit JSDocTag
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocProtectedTag =
        inherit JSDocTag
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocReadonlyTag =
        inherit JSDocTag
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocOverrideTag =
        inherit JSDocTag
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocEnumTag =
        inherit JSDocTag
        inherit Declaration
        abstract kind: SyntaxKind
        abstract parent: JSDoc
        abstract typeExpression: JSDocTypeExpression

    type [<AllowNullLiteral>] JSDocThisTag =
        inherit JSDocTag
        abstract kind: SyntaxKind
        abstract typeExpression: JSDocTypeExpression

    type [<AllowNullLiteral>] JSDocTemplateTag =
        inherit JSDocTag
        abstract kind: SyntaxKind
        abstract ``constraint``: JSDocTypeExpression option
        abstract typeParameters: TypeParameterDeclaration[]

    type [<AllowNullLiteral>] JSDocSeeTag =
        inherit JSDocTag
        abstract kind: SyntaxKind
        abstract name: JSDocNameReference option

    type [<AllowNullLiteral>] JSDocReturnTag =
        inherit JSDocTag
        abstract kind: SyntaxKind
        abstract typeExpression: JSDocTypeExpression option

    type [<AllowNullLiteral>] JSDocTypeTag =
        inherit JSDocTag
        abstract kind: SyntaxKind
        abstract typeExpression: JSDocTypeExpression

    type [<AllowNullLiteral>] JSDocTypedefTag =
        inherit JSDocTag
        inherit NamedDeclaration
        abstract kind: SyntaxKind
        abstract parent: JSDoc
        abstract fullName: U2<JSDocNamespaceDeclaration, Identifier> option
        abstract name: Identifier option
        abstract typeExpression: U2<JSDocTypeExpression, JSDocTypeLiteral> option

    type [<AllowNullLiteral>] JSDocCallbackTag =
        inherit JSDocTag
        inherit NamedDeclaration
        abstract kind: SyntaxKind
        abstract parent: JSDoc
        abstract fullName: U2<JSDocNamespaceDeclaration, Identifier> option
        abstract name: Identifier option
        abstract typeExpression: JSDocSignature

    type [<AllowNullLiteral>] JSDocSignature =
        inherit JSDocType
        inherit Declaration
        abstract kind: SyntaxKind
        abstract typeParameters: JSDocTemplateTag[] option
        abstract parameters: JSDocParameterTag[]
        abstract ``type``: JSDocReturnTag option

    type [<AllowNullLiteral>] JSDocPropertyLikeTag =
        inherit JSDocTag
        inherit Declaration
        abstract parent: JSDoc
        abstract name: EntityName
        abstract typeExpression: JSDocTypeExpression option
        /// Whether the property name came before the type -- non-standard for JSDoc, but Typescript-like
        abstract isNameFirst: bool
        abstract isBracketed: bool

    type [<AllowNullLiteral>] JSDocPropertyTag =
        inherit JSDocPropertyLikeTag
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocParameterTag =
        inherit JSDocPropertyLikeTag
        abstract kind: SyntaxKind

    type [<AllowNullLiteral>] JSDocTypeLiteral =
        inherit JSDocType
        abstract kind: SyntaxKind
        abstract jsDocPropertyTags: JSDocPropertyLikeTag[] option
        /// If true, then this type literal represents an *array* of its type.
        abstract isArrayType: bool

    type [<RequireQualifiedAccess>] FlowFlags =
        | Unreachable = 1
        | Start = 2
        | BranchLabel = 4
        | LoopLabel = 8
        | Assignment = 16
        | TrueCondition = 32
        | FalseCondition = 64
        | SwitchClause = 128
        | ArrayMutation = 256
        | Call = 512
        | ReduceLabel = 1024
        | Referenced = 2048
        | Shared = 4096
        | Label = 12
        | Condition = 96

    type FlowNode =
        U8<FlowStart, FlowLabel, FlowAssignment, FlowCall, FlowCondition, FlowSwitchClause, FlowArrayMutation, FlowReduceLabel>

    type [<AllowNullLiteral>] FlowNodeBase =
        abstract flags: FlowFlags with get, set
        abstract id: float option with get, set

    type [<AllowNullLiteral>] FlowStart =
        inherit FlowNodeBase
        abstract node: U5<FunctionExpression, ArrowFunction, MethodDeclaration, GetAccessorDeclaration, SetAccessorDeclaration> option with get, set

    type [<AllowNullLiteral>] FlowLabel =
        inherit FlowNodeBase
        abstract antecedents: FlowNode[] option with get, set

    type [<AllowNullLiteral>] FlowAssignment =
        inherit FlowNodeBase
        abstract node: U3<Expression, VariableDeclaration, BindingElement> with get, set
        abstract antecedent: FlowNode with get, set

    type [<AllowNullLiteral>] FlowCall =
        inherit FlowNodeBase
        abstract node: CallExpression with get, set
        abstract antecedent: FlowNode with get, set

    type [<AllowNullLiteral>] FlowCondition =
        inherit FlowNodeBase
        abstract node: Expression with get, set
        abstract antecedent: FlowNode with get, set

    type [<AllowNullLiteral>] FlowSwitchClause =
        inherit FlowNodeBase
        abstract switchStatement: SwitchStatement with get, set
        abstract clauseStart: float with get, set
        abstract clauseEnd: float with get, set
        abstract antecedent: FlowNode with get, set

    type [<AllowNullLiteral>] FlowArrayMutation =
        inherit FlowNodeBase
        abstract node: U2<CallExpression, BinaryExpression> with get, set
        abstract antecedent: FlowNode with get, set

    type [<AllowNullLiteral>] FlowReduceLabel =
        inherit FlowNodeBase
        abstract target: FlowLabel with get, set
        abstract antecedents: FlowNode[] with get, set
        abstract antecedent: FlowNode with get, set

    type FlowType =
        U2<Type, IncompleteType>

    type [<AllowNullLiteral>] IncompleteType =
        abstract flags: TypeFlags with get, set
        abstract ``type``: Type with get, set

    type [<AllowNullLiteral>] AmdDependency =
        abstract path: string with get, set
        abstract name: string option with get, set

    type [<AllowNullLiteral>] SourceFile =
        inherit Declaration
        abstract kind: SyntaxKind
        abstract statements: Statement[]
        abstract endOfFileToken: Token<SyntaxKind>
        abstract fileName: string with get, set
        abstract text: string with get, set
        abstract amdDependencies: AmdDependency[] with get, set
        abstract moduleName: string option with get, set
        abstract referencedFiles: FileReference[] with get, set
        abstract typeReferenceDirectives: FileReference[] with get, set
        abstract libReferenceDirectives: FileReference[] with get, set
        abstract languageVariant: LanguageVariant with get, set
        abstract isDeclarationFile: bool with get, set
        /// lib.d.ts should have a reference comment like
        ///
        ///   /// <reference no-default-lib="true"/>
        ///
        /// If any other file has this comment, it signals not to include lib.d.ts
        /// because this containing file is intended to act as a default library.
        abstract hasNoDefaultLib: bool with get, set
        abstract languageVersion: ScriptTarget with get, set
        /// <summary>
        /// When <c>module</c> is <c>Node12</c> or <c>NodeNext</c>, this field controls whether the
        /// source file in question is an ESNext-output-format file, or a CommonJS-output-format
        /// module. This is derived by the module resolver as it looks up the file, since
        /// it is derived from either the file extension of the module, or the containing
        /// <c>package.json</c> context, and affects both checking and emit.
        ///
        /// It is _public_ so that (pre)transformers can set this field,
        /// since it switches the builtin <c>node</c> module transform. Generally speaking, if unset,
        /// the field is treated as though it is <c>ModuleKind.CommonJS</c>.
        /// </summary>
        abstract impliedNodeFormat: ModuleKind option with get, set
        abstract getLineAndCharacterOfPosition: pos: float -> LineAndCharacter
        abstract getLineEndOfPosition: pos: float -> float
        abstract getLineStarts: unit -> float[]
        abstract getPositionOfLineAndCharacter: line: float * character: float -> float
        abstract update: newText: string * textChangeRange: TextChangeRange -> SourceFile

    type [<AllowNullLiteral>] Bundle =
        inherit Node
        abstract kind: SyntaxKind
        abstract prepends: U2<InputFiles, UnparsedSource>[]
        abstract sourceFiles: SourceFile[]

    type [<AllowNullLiteral>] InputFiles =
        inherit Node
        abstract kind: SyntaxKind
        abstract javascriptPath: string option with get, set
        abstract javascriptText: string with get, set
        abstract javascriptMapPath: string option with get, set
        abstract javascriptMapText: string option with get, set
        abstract declarationPath: string option with get, set
        abstract declarationText: string with get, set
        abstract declarationMapPath: string option with get, set
        abstract declarationMapText: string option with get, set

    type [<AllowNullLiteral>] UnparsedSource =
        inherit Node
        abstract kind: SyntaxKind
        abstract fileName: string with get, set
        abstract text: string with get, set
        abstract prologues: UnparsedPrologue[]
        abstract helpers: UnscopedEmitHelper[] option with get, set
        abstract referencedFiles: FileReference[] with get, set
        abstract typeReferenceDirectives: string[] option with get, set
        abstract libReferenceDirectives: FileReference[] with get, set
        abstract hasNoDefaultLib: bool option with get, set
        abstract sourceMapPath: string option with get, set
        abstract sourceMapText: string option with get, set
        abstract syntheticReferences: UnparsedSyntheticReference[] option
        abstract texts: UnparsedSourceText[]

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] UnparsedSourceText =
        | [<CompiledValue 299>] UnparsedPrepend of UnparsedPrepend
        | [<CompiledValue 300>] UnparsedTextLike of UnparsedTextLike
        | [<CompiledValue 301>] UnparsedTextLike' of UnparsedTextLike

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] UnparsedNode =
        | [<CompiledValue 298>] UnparsedPrologue of UnparsedPrologue
        | [<CompiledValue 299>] UnparsedPrepend of UnparsedPrepend
        | [<CompiledValue 300>] UnparsedTextLike of UnparsedTextLike
        | [<CompiledValue 301>] UnparsedTextLike' of UnparsedTextLike
        | [<CompiledValue 302>] UnparsedSyntheticReference of UnparsedSyntheticReference

    type [<AllowNullLiteral>] UnparsedSection =
        inherit Node
        abstract kind: SyntaxKind
        abstract parent: UnparsedSource
        abstract data: string option

    type [<AllowNullLiteral>] UnparsedPrologue =
        inherit UnparsedSection
        abstract kind: SyntaxKind
        abstract parent: UnparsedSource
        abstract data: string

    type [<AllowNullLiteral>] UnparsedPrepend =
        inherit UnparsedSection
        abstract kind: SyntaxKind
        abstract parent: UnparsedSource
        abstract data: string
        abstract texts: UnparsedTextLike[]

    type [<AllowNullLiteral>] UnparsedTextLike =
        inherit UnparsedSection
        abstract kind: SyntaxKind
        abstract parent: UnparsedSource

    type [<AllowNullLiteral>] UnparsedSyntheticReference =
        inherit UnparsedSection
        abstract kind: SyntaxKind
        abstract parent: UnparsedSource

    type [<AllowNullLiteral>] JsonSourceFile =
        inherit SourceFile
        abstract statements: JsonObjectExpressionStatement[]

    type [<AllowNullLiteral>] TsConfigSourceFile =
        inherit JsonSourceFile
        abstract extendedSourceFiles: string[] option with get, set

    type [<AllowNullLiteral>] JsonMinusNumericLiteral =
        inherit PrefixUnaryExpression
        abstract kind: SyntaxKind
        abstract operator: SyntaxKind
        abstract operand: NumericLiteral

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] JsonObjectExpression =
        | [<CompiledValue 8>] NumericLiteral of NumericLiteral
        | [<CompiledValue 10>] StringLiteral of StringLiteral
        | [<CompiledValue 95>] FalseLiteral of FalseLiteral
        | [<CompiledValue 104>] NullLiteral of NullLiteral
        | [<CompiledValue 110>] TrueLiteral of TrueLiteral
        | [<CompiledValue 203>] ArrayLiteralExpression of ArrayLiteralExpression
        | [<CompiledValue 204>] ObjectLiteralExpression of ObjectLiteralExpression
        | [<CompiledValue 218>] JsonMinusNumericLiteral of JsonMinusNumericLiteral

    type [<AllowNullLiteral>] JsonObjectExpressionStatement =
        inherit ExpressionStatement
        abstract expression: JsonObjectExpression

    type [<AllowNullLiteral>] ScriptReferenceHost =
        abstract getCompilerOptions: unit -> CompilerOptions
        abstract getSourceFile: fileName: string -> SourceFile option
        abstract getSourceFileByPath: path: Path -> SourceFile option
        abstract getCurrentDirectory: unit -> string

    type [<AllowNullLiteral>] ParseConfigHost =
        abstract useCaseSensitiveFileNames: bool with get, set
        abstract readDirectory: rootDir: string * extensions: string[] * excludes: string[] option * includes: string[] * ?depth: float -> string[]
        /// <summary>Gets a value indicating whether the specified path exists and is a file.</summary>
        /// <param name="path">The path to test.</param>
        abstract fileExists: path: string -> bool
        abstract readFile: path: string -> string option
        abstract trace: s: string -> unit

    /// Branded string for keeping track of when we've turned an ambiguous path
    /// specified like "./blah" to an absolute path to an actual
    /// tsconfig file, e.g. "/root/blah/tsconfig.json"
    type [<AllowNullLiteral>] ResolvedConfigFileName =
        interface end

    type [<AllowNullLiteral>] WriteFileCallback =
        [<Emit "$0($1...)">] abstract Invoke: fileName: string * data: string * writeByteOrderMark: bool * ?onError: (string -> unit) * ?sourceFiles: SourceFile[] -> unit

    type [<AllowNullLiteral>] OperationCanceledException =
        interface end

    type [<AllowNullLiteral>] OperationCanceledExceptionStatic =
        [<EmitConstructor>] abstract Create: unit -> OperationCanceledException

    type [<AllowNullLiteral>] CancellationToken =
        abstract isCancellationRequested: unit -> bool
        /// <exception cref="">OperationCanceledException if isCancellationRequested is true</exception>
        abstract throwIfCancellationRequested: unit -> unit

    type [<AllowNullLiteral>] Program =
        inherit ScriptReferenceHost
        abstract getCurrentDirectory: unit -> string
        /// Get a list of root file names that were passed to a 'createProgram'
        abstract getRootFileNames: unit -> string[]
        /// Get a list of files in the program
        abstract getSourceFiles: unit -> SourceFile[]
        /// Emits the JavaScript and declaration files.  If targetSourceFile is not specified, then
        /// the JavaScript and declaration files will be produced for all the files in this program.
        /// If targetSourceFile is specified, then only the JavaScript and declaration for that
        /// specific file will be generated.
        ///
        /// If writeFile is not specified then the writeFile callback from the compiler host will be
        /// used for writing the JavaScript and declaration files.  Otherwise, the writeFile parameter
        /// will be invoked when writing the JavaScript and declaration files.
        abstract emit: ?targetSourceFile: SourceFile * ?writeFile: WriteFileCallback * ?cancellationToken: CancellationToken * ?emitOnlyDtsFiles: bool * ?customTransformers: CustomTransformers -> EmitResult
        abstract getOptionsDiagnostics: ?cancellationToken: CancellationToken -> Diagnostic[]
        abstract getGlobalDiagnostics: ?cancellationToken: CancellationToken -> Diagnostic[]
        abstract getSyntacticDiagnostics: ?sourceFile: SourceFile * ?cancellationToken: CancellationToken -> DiagnosticWithLocation[]
        /// The first time this is called, it will return global diagnostics (no location).
        abstract getSemanticDiagnostics: ?sourceFile: SourceFile * ?cancellationToken: CancellationToken -> Diagnostic[]
        abstract getDeclarationDiagnostics: ?sourceFile: SourceFile * ?cancellationToken: CancellationToken -> DiagnosticWithLocation[]
        abstract getConfigFileParsingDiagnostics: unit -> Diagnostic[]
        /// Gets a type checker that can be used to semantically analyze source files in the program.
        abstract getTypeChecker: unit -> TypeChecker
        abstract getNodeCount: unit -> float
        abstract getIdentifierCount: unit -> float
        abstract getSymbolCount: unit -> float
        abstract getTypeCount: unit -> float
        abstract getInstantiationCount: unit -> float
        abstract getRelationCacheSizes: unit -> {| assignable: float; identity: float; subtype: float; strictSubtype: float |}
        abstract isSourceFileFromExternalLibrary: file: SourceFile -> bool
        abstract isSourceFileDefaultLibrary: file: SourceFile -> bool
        abstract getProjectReferences: unit -> ProjectReference[] option
        abstract getResolvedProjectReferences: unit -> ResolvedProjectReference option[] option

    type [<AllowNullLiteral>] ResolvedProjectReference =
        abstract commandLine: ParsedCommandLine with get, set
        abstract sourceFile: SourceFile with get, set
        abstract references: ResolvedProjectReference option[] option with get, set

    type [<AllowNullLiteral>] CustomTransformerFactory =
        [<Emit "$0($1...)">] abstract Invoke: context: TransformationContext -> CustomTransformer

    type [<AllowNullLiteral>] CustomTransformer =
        abstract transformSourceFile: node: SourceFile -> SourceFile
        abstract transformBundle: node: Bundle -> Bundle

    type [<AllowNullLiteral>] CustomTransformers =
        /// Custom transformers to evaluate before built-in .js transformations.
        abstract before: U2<TransformerFactory<SourceFile>, CustomTransformerFactory>[] option with get, set
        /// Custom transformers to evaluate after built-in .js transformations.
        abstract after: U2<TransformerFactory<SourceFile>, CustomTransformerFactory>[] option with get, set
        /// Custom transformers to evaluate after built-in .d.ts transformations.
        abstract afterDeclarations: U2<TransformerFactory<U2<Bundle, SourceFile>>, CustomTransformerFactory>[] option with get, set

    type [<AllowNullLiteral>] SourceMapSpan =
        /// Line number in the .js file.
        abstract emittedLine: float with get, set
        /// Column number in the .js file.
        abstract emittedColumn: float with get, set
        /// Line number in the .ts file.
        abstract sourceLine: float with get, set
        /// Column number in the .ts file.
        abstract sourceColumn: float with get, set
        /// Optional name (index into names array) associated with this span.
        abstract nameIndex: float option with get, set
        /// .ts file (index into sources array) associated with this span
        abstract sourceIndex: float with get, set

    /// Return code used by getEmitOutput function to indicate status of the function
    type [<RequireQualifiedAccess>] ExitStatus =
        | Success = 0
        | DiagnosticsPresent_OutputsSkipped = 1
        | DiagnosticsPresent_OutputsGenerated = 2
        | InvalidProject_OutputsSkipped = 3
        | ProjectReferenceCycle_OutputsSkipped = 4
        /// <deprecated>Use ProjectReferenceCycle_OutputsSkipped instead.</deprecated>
        | ProjectReferenceCycle_OutputsSkupped = 4

    type [<AllowNullLiteral>] EmitResult =
        abstract emitSkipped: bool with get, set
        /// Contains declaration emit diagnostics
        abstract diagnostics: Diagnostic[] with get, set
        abstract emittedFiles: string[] option with get, set

    type [<AllowNullLiteral>] TypeChecker =
        abstract getTypeOfSymbolAtLocation: symbol: Symbol * node: Node -> Type
        abstract getDeclaredTypeOfSymbol: symbol: Symbol -> Type
        abstract getPropertiesOfType: ``type``: Type -> Symbol[]
        abstract getPropertyOfType: ``type``: Type * propertyName: string -> Symbol option
        abstract getPrivateIdentifierPropertyOfType: leftType: Type * name: string * location: Node -> Symbol option
        abstract getIndexInfoOfType: ``type``: Type * kind: IndexKind -> IndexInfo option
        abstract getIndexInfosOfType: ``type``: Type -> IndexInfo[]
        abstract getSignaturesOfType: ``type``: Type * kind: SignatureKind -> Signature[]
        abstract getIndexTypeOfType: ``type``: Type * kind: IndexKind -> Type option
        abstract getBaseTypes: ``type``: InterfaceType -> BaseType[]
        abstract getBaseTypeOfLiteralType: ``type``: Type -> Type
        abstract getWidenedType: ``type``: Type -> Type
        abstract getReturnTypeOfSignature: signature: Signature -> Type
        abstract getNullableType: ``type``: Type * flags: TypeFlags -> Type
        abstract getNonNullableType: ``type``: Type -> Type
        abstract getTypeArguments: ``type``: TypeReference -> Type[]
        /// Note that the resulting nodes cannot be checked.
        abstract typeToTypeNode: ``type``: Type * enclosingDeclaration: Node option * flags: NodeBuilderFlags option -> TypeNode option
        /// Note that the resulting nodes cannot be checked.
        abstract signatureToSignatureDeclaration: signature: Signature * kind: SyntaxKind * enclosingDeclaration: Node option * flags: NodeBuilderFlags option -> obj option
        /// Note that the resulting nodes cannot be checked.
        abstract indexInfoToIndexSignatureDeclaration: indexInfo: IndexInfo * enclosingDeclaration: Node option * flags: NodeBuilderFlags option -> IndexSignatureDeclaration option
        /// Note that the resulting nodes cannot be checked.
        abstract symbolToEntityName: symbol: Symbol * meaning: SymbolFlags * enclosingDeclaration: Node option * flags: NodeBuilderFlags option -> EntityName option
        /// Note that the resulting nodes cannot be checked.
        abstract symbolToExpression: symbol: Symbol * meaning: SymbolFlags * enclosingDeclaration: Node option * flags: NodeBuilderFlags option -> Expression option
        /// Note that the resulting nodes cannot be checked.
        abstract symbolToTypeParameterDeclarations: symbol: Symbol * enclosingDeclaration: Node option * flags: NodeBuilderFlags option -> TypeParameterDeclaration[] option
        /// Note that the resulting nodes cannot be checked.
        abstract symbolToParameterDeclaration: symbol: Symbol * enclosingDeclaration: Node option * flags: NodeBuilderFlags option -> ParameterDeclaration option
        /// Note that the resulting nodes cannot be checked.
        abstract typeParameterToDeclaration: parameter: TypeParameter * enclosingDeclaration: Node option * flags: NodeBuilderFlags option -> TypeParameterDeclaration option
        abstract getSymbolsInScope: location: Node * meaning: SymbolFlags -> Symbol[]
        abstract getSymbolAtLocation: node: Node -> Symbol option
        abstract getSymbolsOfParameterPropertyDeclaration: parameter: ParameterDeclaration * parameterName: string -> Symbol[]
        /// The function returns the value (local variable) symbol of an identifier in the short-hand property assignment.
        /// This is necessary as an identifier in short-hand property assignment can contains two meaning: property name and property value.
        abstract getShorthandAssignmentValueSymbol: location: Node option -> Symbol option
        abstract getExportSpecifierLocalTargetSymbol: location: U2<ExportSpecifier, Identifier> -> Symbol option
        /// <summary>
        /// If a symbol is a local symbol with an associated exported symbol, returns the exported symbol.
        /// Otherwise returns its input.
        /// For example, at <c>export type T = number;</c>:
        ///      - <c>getSymbolAtLocation</c> at the location <c>T</c> will return the exported symbol for <c>T</c>.
        ///      - But the result of <c>getSymbolsInScope</c> will contain the *local* symbol for <c>T</c>, not the exported symbol.
        ///      - Calling <c>getExportSymbolOfSymbol</c> on that local symbol will return the exported symbol.
        /// </summary>
        abstract getExportSymbolOfSymbol: symbol: Symbol -> Symbol
        abstract getPropertySymbolOfDestructuringAssignment: location: Identifier -> Symbol option
        abstract getTypeOfAssignmentPattern: pattern: AssignmentPattern -> Type
        abstract getTypeAtLocation: node: Node -> Type
        abstract getTypeFromTypeNode: node: TypeNode -> Type
        abstract signatureToString: signature: Signature * ?enclosingDeclaration: Node * ?flags: TypeFormatFlags * ?kind: SignatureKind -> string
        abstract typeToString: ``type``: Type * ?enclosingDeclaration: Node * ?flags: TypeFormatFlags -> string
        abstract symbolToString: symbol: Symbol * ?enclosingDeclaration: Node * ?meaning: SymbolFlags * ?flags: SymbolFormatFlags -> string
        abstract typePredicateToString: predicate: TypePredicate * ?enclosingDeclaration: Node * ?flags: TypeFormatFlags -> string
        abstract getFullyQualifiedName: symbol: Symbol -> string
        abstract getAugmentedPropertiesOfType: ``type``: Type -> Symbol[]
        abstract getRootSymbols: symbol: Symbol -> Symbol[]
        abstract getSymbolOfExpando: node: Node * allowDeclaration: bool -> Symbol option
        abstract getContextualType: node: Expression -> Type option
        /// <summary>
        /// returns unknownSignature in the case of an error.
        /// returns undefined if the node is not valid.
        /// </summary>
        /// <param name="argumentCount">Apparent number of arguments, passed in case of a possibly incomplete call. This should come from an ArgumentListInfo. See <c>signatureHelp.ts</c>.</param>
        abstract getResolvedSignature: node: CallLikeExpression * ?candidatesOutArray: Signature[] * ?argumentCount: float -> Signature option
        abstract getSignatureFromDeclaration: declaration: SignatureDeclaration -> Signature option
        abstract isImplementationOfOverload: node: SignatureDeclaration -> bool option
        abstract isUndefinedSymbol: symbol: Symbol -> bool
        abstract isArgumentsSymbol: symbol: Symbol -> bool
        abstract isUnknownSymbol: symbol: Symbol -> bool
        abstract getConstantValue: node: U3<EnumMember, PropertyAccessExpression, ElementAccessExpression> -> U2<string, float> option
        abstract isValidPropertyAccess: node: U3<PropertyAccessExpression, QualifiedName, ImportTypeNode> * propertyName: string -> bool
        /// Follow all aliases to get the original symbol.
        abstract getAliasedSymbol: symbol: Symbol -> Symbol
        /// Follow a *single* alias to get the immediately aliased symbol.
        abstract getImmediateAliasedSymbol: symbol: Symbol -> Symbol option
        abstract getExportsOfModule: moduleSymbol: Symbol -> Symbol[]
        abstract getJsxIntrinsicTagNamesAt: location: Node -> Symbol[]
        abstract isOptionalParameter: node: ParameterDeclaration -> bool
        abstract getAmbientModules: unit -> Symbol[]
        abstract tryGetMemberInModuleExports: memberName: string * moduleSymbol: Symbol -> Symbol option
        abstract getApparentType: ``type``: Type -> Type
        abstract getBaseConstraintOfType: ``type``: Type -> Type option
        abstract getDefaultFromTypeParameter: ``type``: Type -> Type option
        abstract getTypePredicateOfSignature: signature: Signature -> TypePredicate option
        /// Depending on the operation performed, it may be appropriate to throw away the checker
        /// if the cancellation token is triggered. Typically, if it is used for error checking
        /// and the operation is cancelled, then it should be discarded, otherwise it is safe to keep.
        abstract runWithCancellationToken: token: CancellationToken * cb: (TypeChecker -> 'T) -> 'T

    type [<RequireQualifiedAccess>] NodeBuilderFlags =
        | None = 0
        | NoTruncation = 1
        | WriteArrayAsGenericType = 2
        | GenerateNamesForShadowedTypeParams = 4
        | UseStructuralFallback = 8
        | ForbidIndexedAccessSymbolReferences = 16
        | WriteTypeArgumentsOfSignature = 32
        | UseFullyQualifiedType = 64
        | UseOnlyExternalAliasing = 128
        | SuppressAnyReturnType = 256
        | WriteTypeParametersInQualifiedName = 512
        | MultilineObjectLiterals = 1024
        | WriteClassExpressionAsTypeLiteral = 2048
        | UseTypeOfFunction = 4096
        | OmitParameterModifiers = 8192
        | UseAliasDefinedOutsideCurrentScope = 16384
        | UseSingleQuotesForStringLiteralType = 268435456
        | NoTypeReduction = 536870912
        | NoUndefinedOptionalParameterType = 1073741824
        | AllowThisInObjectLiteral = 32768
        | AllowQualifiedNameInPlaceOfIdentifier = 65536
        /// <deprecated>AllowQualifedNameInPlaceOfIdentifier. Use AllowQualifiedNameInPlaceOfIdentifier instead.</deprecated>
        | AllowQualifedNameInPlaceOfIdentifier = 65536
        | AllowAnonymousIdentifier = 131072
        | AllowEmptyUnionOrIntersection = 262144
        | AllowEmptyTuple = 524288
        | AllowUniqueESSymbolType = 1048576
        | AllowEmptyIndexInfoType = 2097152
        | AllowNodeModulesRelativePaths = 67108864
        | IgnoreErrors = 70221824
        | InObjectTypeLiteral = 4194304
        | InTypeAlias = 8388608
        | InInitialEntityName = 16777216

    type [<RequireQualifiedAccess>] TypeFormatFlags =
        | None = 0
        | NoTruncation = 1
        | WriteArrayAsGenericType = 2
        | UseStructuralFallback = 8
        | WriteTypeArgumentsOfSignature = 32
        | UseFullyQualifiedType = 64
        | SuppressAnyReturnType = 256
        | MultilineObjectLiterals = 1024
        | WriteClassExpressionAsTypeLiteral = 2048
        | UseTypeOfFunction = 4096
        | OmitParameterModifiers = 8192
        | UseAliasDefinedOutsideCurrentScope = 16384
        | UseSingleQuotesForStringLiteralType = 268435456
        | NoTypeReduction = 536870912
        | AllowUniqueESSymbolType = 1048576
        | AddUndefined = 131072
        | WriteArrowStyleSignature = 262144
        | InArrayType = 524288
        | InElementType = 2097152
        | InFirstTypeArgument = 4194304
        | InTypeAlias = 8388608
        /// <deprecated />
        | WriteOwnNameForAnyLike = 0
        | NodeBuilderFlagsMask = 814775659

    type [<RequireQualifiedAccess>] SymbolFormatFlags =
        | None = 0
        | WriteTypeParametersOrArguments = 1
        | UseOnlyExternalAliasing = 2
        | AllowAnyNodeKind = 4
        | UseAliasDefinedOutsideCurrentScope = 8

    type [<RequireQualifiedAccess>] TypePredicateKind =
        | This = 0
        | Identifier = 1
        | AssertsThis = 2
        | AssertsIdentifier = 3

    type [<AllowNullLiteral>] TypePredicateBase =
        abstract kind: TypePredicateKind with get, set
        abstract ``type``: Type option with get, set

    type [<AllowNullLiteral>] ThisTypePredicate =
        inherit TypePredicateBase
        abstract kind: TypePredicateKind with get, set
        abstract parameterName: obj with get, set
        abstract parameterIndex: obj with get, set
        abstract ``type``: Type with get, set

    type [<AllowNullLiteral>] IdentifierTypePredicate =
        inherit TypePredicateBase
        abstract kind: TypePredicateKind with get, set
        abstract parameterName: string with get, set
        abstract parameterIndex: float with get, set
        abstract ``type``: Type with get, set

    type [<AllowNullLiteral>] AssertsThisTypePredicate =
        inherit TypePredicateBase
        abstract kind: TypePredicateKind with get, set
        abstract parameterName: obj with get, set
        abstract parameterIndex: obj with get, set
        abstract ``type``: Type option with get, set

    type [<AllowNullLiteral>] AssertsIdentifierTypePredicate =
        inherit TypePredicateBase
        abstract kind: TypePredicateKind with get, set
        abstract parameterName: string with get, set
        abstract parameterIndex: float with get, set
        abstract ``type``: Type option with get, set

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] TypePredicate =
        | [<CompiledValue 0>] ThisTypePredicate of ThisTypePredicate
        | [<CompiledValue 1>] IdentifierTypePredicate of IdentifierTypePredicate
        | [<CompiledValue 2>] AssertsThisTypePredicate of AssertsThisTypePredicate
        | [<CompiledValue 3>] AssertsIdentifierTypePredicate of AssertsIdentifierTypePredicate

    type [<RequireQualifiedAccess>] SymbolFlags =
        | None = 0
        | FunctionScopedVariable = 1
        | BlockScopedVariable = 2
        | Property = 4
        | EnumMember = 8
        | Function = 16
        | Class = 32
        | Interface = 64
        | ConstEnum = 128
        | RegularEnum = 256
        | ValueModule = 512
        | NamespaceModule = 1024
        | TypeLiteral = 2048
        | ObjectLiteral = 4096
        | Method = 8192
        | Constructor = 16384
        | GetAccessor = 32768
        | SetAccessor = 65536
        | Signature = 131072
        | TypeParameter = 262144
        | TypeAlias = 524288
        | ExportValue = 1048576
        | Alias = 2097152
        | Prototype = 4194304
        | ExportStar = 8388608
        | Optional = 16777216
        | Transient = 33554432
        | Assignment = 67108864
        | ModuleExports = 134217728
        | Enum = 384
        | Variable = 3
        | Value = 111551
        | Type = 788968
        | Namespace = 1920
        | Module = 1536
        | Accessor = 98304
        | FunctionScopedVariableExcludes = 111550
        | BlockScopedVariableExcludes = 111551
        | ParameterExcludes = 111551
        | PropertyExcludes = 0
        | EnumMemberExcludes = 900095
        | FunctionExcludes = 110991
        | ClassExcludes = 899503
        | InterfaceExcludes = 788872
        | RegularEnumExcludes = 899327
        | ConstEnumExcludes = 899967
        | ValueModuleExcludes = 110735
        | NamespaceModuleExcludes = 0
        | MethodExcludes = 103359
        | GetAccessorExcludes = 46015
        | SetAccessorExcludes = 78783
        | TypeParameterExcludes = 526824
        | TypeAliasExcludes = 788968
        | AliasExcludes = 2097152
        | ModuleMember = 2623475
        | ExportHasLocal = 944
        | BlockScoped = 418
        | PropertyOrAccessor = 98308
        | ClassMember = 106500

    type [<AllowNullLiteral>] Symbol =
        abstract flags: SymbolFlags with get, set
        abstract escapedName: __String with get, set
        abstract declarations: Declaration[] option with get, set
        abstract valueDeclaration: Declaration option with get, set
        abstract members: SymbolTable option with get, set
        abstract exports: SymbolTable option with get, set
        abstract globalExports: SymbolTable option with get, set
        abstract name: string
        abstract getFlags: unit -> SymbolFlags
        abstract getEscapedName: unit -> __String
        abstract getName: unit -> string
        abstract getDeclarations: unit -> Declaration[] option
        abstract getDocumentationComment: typeChecker: TypeChecker option -> SymbolDisplayPart[]
        abstract getJsDocTags: ?checker: TypeChecker -> JSDocTagInfo[]

    type [<StringEnum>] [<RequireQualifiedAccess>] InternalSymbolName =
        | [<CompiledName "__call">] Call
        | [<CompiledName "__constructor">] Constructor
        | [<CompiledName "__new">] New
        | [<CompiledName "__index">] Index
        | [<CompiledName "__export">] ExportStar
        | [<CompiledName "__global">] Global
        | [<CompiledName "__missing">] Missing
        | [<CompiledName "__type">] Type
        | [<CompiledName "__object">] Object
        | [<CompiledName "__jsxAttributes">] JSXAttributes
        | [<CompiledName "__class">] Class
        | [<CompiledName "__function">] Function
        | [<CompiledName "__computed">] Computed
        | [<CompiledName "__resolving__">] Resolving
        | [<CompiledName "export=">] ExportEquals
        | Default
        | This

    /// This represents a string whose leading underscore have been escaped by adding extra leading underscores.
    /// The shape of this brand is rather unique compared to others we've used.
    /// Instead of just an intersection of a string and an object, it is that union-ed
    /// with an intersection of void and an object. This makes it wholly incompatible
    /// with a normal string (which is good, it cannot be misused on assignment or on usage),
    /// while still being comparable with a normal string via === (also good) and castable from a string.
    type __String =
        U2<obj, InternalSymbolName>

    /// <summary>ReadonlyMap where keys are <c>__String</c>s.</summary>
    type [<AllowNullLiteral>] ReadonlyUnderscoreEscapedMap<'T> =
        inherit ReadonlyESMap<__String, 'T>

    /// <summary>Map where keys are <c>__String</c>s.</summary>
    type [<AllowNullLiteral>] UnderscoreEscapedMap<'T> =
        inherit ESMap<__String, 'T>
        inherit ReadonlyUnderscoreEscapedMap<'T>

    /// SymbolTable based on ES6 Map interface.
    type SymbolTable =
        UnderscoreEscapedMap<Symbol>

    type [<RequireQualifiedAccess>] TypeFlags =
        | Any = 1
        | Unknown = 2
        | String = 4
        | Number = 8
        | Boolean = 16
        | Enum = 32
        | BigInt = 64
        | StringLiteral = 128
        | NumberLiteral = 256
        | BooleanLiteral = 512
        | EnumLiteral = 1024
        | BigIntLiteral = 2048
        | ESSymbol = 4096
        | UniqueESSymbol = 8192
        | Void = 16384
        | Undefined = 32768
        | Null = 65536
        | Never = 131072
        | TypeParameter = 262144
        | Object = 524288
        | Union = 1048576
        | Intersection = 2097152
        | Index = 4194304
        | IndexedAccess = 8388608
        | Conditional = 16777216
        | Substitution = 33554432
        | NonPrimitive = 67108864
        | TemplateLiteral = 134217728
        | StringMapping = 268435456
        | Literal = 2944
        | Unit = 109440
        | StringOrNumberLiteral = 384
        | PossiblyFalsy = 117724
        | StringLike = 402653316
        | NumberLike = 296
        | BigIntLike = 2112
        | BooleanLike = 528
        | EnumLike = 1056
        | ESSymbolLike = 12288
        | VoidLike = 49152
        | UnionOrIntersection = 3145728
        | StructuredType = 3670016
        | TypeVariable = 8650752
        | InstantiableNonPrimitive = 58982400
        | InstantiablePrimitive = 406847488
        | Instantiable = 465829888
        | StructuredOrInstantiable = 469499904
        | Narrowable = 536624127

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] DestructuringPattern =
        | [<CompiledValue 200>] ObjectBindingPattern of ObjectBindingPattern
        | [<CompiledValue 201>] ArrayBindingPattern of ArrayBindingPattern
        | [<CompiledValue 203>] ArrayLiteralExpression of ArrayLiteralExpression
        | [<CompiledValue 204>] ObjectLiteralExpression of ObjectLiteralExpression

    type [<AllowNullLiteral>] Type =
        abstract flags: TypeFlags with get, set
        abstract symbol: Symbol with get, set
        abstract pattern: DestructuringPattern option with get, set
        abstract aliasSymbol: Symbol option with get, set
        abstract aliasTypeArguments: Type[] option with get, set
        abstract getFlags: unit -> TypeFlags
        abstract getSymbol: unit -> Symbol option
        abstract getProperties: unit -> Symbol[]
        abstract getProperty: propertyName: string -> Symbol option
        abstract getApparentProperties: unit -> Symbol[]
        abstract getCallSignatures: unit -> Signature[]
        abstract getConstructSignatures: unit -> Signature[]
        abstract getStringIndexType: unit -> Type option
        abstract getNumberIndexType: unit -> Type option
        abstract getBaseTypes: unit -> BaseType[] option
        abstract getNonNullableType: unit -> Type
        abstract getConstraint: unit -> Type option
        abstract getDefault: unit -> Type option
        abstract isUnion: unit -> bool
        abstract isIntersection: unit -> bool
        abstract isUnionOrIntersection: unit -> bool
        abstract isLiteral: unit -> bool
        abstract isStringLiteral: unit -> bool
        abstract isNumberLiteral: unit -> bool
        abstract isTypeParameter: unit -> bool
        abstract isClassOrInterface: unit -> bool
        abstract isClass: unit -> bool
        abstract isIndexType: unit -> bool

    type [<AllowNullLiteral>] LiteralType =
        inherit Type
        abstract value: U3<string, float, PseudoBigInt> with get, set
        abstract freshType: LiteralType with get, set
        abstract regularType: LiteralType with get, set

    type [<AllowNullLiteral>] UniqueESSymbolType =
        inherit Type
        abstract symbol: Symbol with get, set
        abstract escapedName: __String with get, set

    type [<AllowNullLiteral>] StringLiteralType =
        inherit LiteralType
        abstract value: string with get, set

    type [<AllowNullLiteral>] NumberLiteralType =
        inherit LiteralType
        abstract value: float with get, set

    type [<AllowNullLiteral>] BigIntLiteralType =
        inherit LiteralType
        abstract value: PseudoBigInt with get, set

    type [<AllowNullLiteral>] EnumType =
        inherit Type

    type [<RequireQualifiedAccess>] ObjectFlags =
        | Class = 1
        | Interface = 2
        | Reference = 4
        | Tuple = 8
        | Anonymous = 16
        | Mapped = 32
        | Instantiated = 64
        | ObjectLiteral = 128
        | EvolvingArray = 256
        | ObjectLiteralPatternWithComputedProperties = 512
        | ReverseMapped = 1024
        | JsxAttributes = 2048
        | MarkerType = 4096
        | JSLiteral = 8192
        | FreshLiteral = 16384
        | ArrayLiteral = 32768
        | ClassOrInterface = 3
        | ContainsSpread = 4194304
        | ObjectRestType = 8388608

    type [<AllowNullLiteral>] ObjectType =
        inherit Type
        abstract objectFlags: ObjectFlags with get, set

    /// Class and interface types (ObjectFlags.Class and ObjectFlags.Interface).
    type [<AllowNullLiteral>] InterfaceType =
        inherit ObjectType
        abstract typeParameters: TypeParameter[] option with get, set
        abstract outerTypeParameters: TypeParameter[] option with get, set
        abstract localTypeParameters: TypeParameter[] option with get, set
        abstract thisType: TypeParameter option with get, set

    type BaseType =
        U3<ObjectType, IntersectionType, TypeVariable>

    type [<AllowNullLiteral>] InterfaceTypeWithDeclaredMembers =
        inherit InterfaceType
        abstract declaredProperties: Symbol[] with get, set
        abstract declaredCallSignatures: Signature[] with get, set
        abstract declaredConstructSignatures: Signature[] with get, set
        abstract declaredIndexInfos: IndexInfo[] with get, set

    /// Type references (ObjectFlags.Reference). When a class or interface has type parameters or
    /// a "this" type, references to the class or interface are made using type references. The
    /// typeArguments property specifies the types to substitute for the type parameters of the
    /// class or interface and optionally includes an extra element that specifies the type to
    /// substitute for "this" in the resulting instantiation. When no extra argument is present,
    /// the type reference itself is substituted for "this". The typeArguments property is undefined
    /// if the class or interface has no type parameters and the reference isn't specifying an
    /// explicit "this" argument.
    type [<AllowNullLiteral>] TypeReference =
        inherit ObjectType
        abstract target: GenericType with get, set
        abstract node: U3<TypeReferenceNode, ArrayTypeNode, TupleTypeNode> option with get, set
        abstract typeArguments: Type[] option with get, set

    type [<AllowNullLiteral>] DeferredTypeReference =
        inherit TypeReference

    type [<AllowNullLiteral>] GenericType =
        inherit InterfaceType
        inherit TypeReference

    type [<RequireQualifiedAccess>] ElementFlags =
        | Required = 1
        | Optional = 2
        | Rest = 4
        | Variadic = 8
        | Fixed = 3
        | Variable = 12
        | NonRequired = 14
        | NonRest = 11

    type [<AllowNullLiteral>] TupleType =
        inherit GenericType
        abstract elementFlags: ElementFlags[] with get, set
        abstract minLength: float with get, set
        abstract fixedLength: float with get, set
        abstract hasRestElement: bool with get, set
        abstract combinedFlags: ElementFlags with get, set
        abstract readonly: bool with get, set
        abstract labeledElementDeclarations: U2<NamedTupleMember, ParameterDeclaration>[] option with get, set

    type [<AllowNullLiteral>] TupleTypeReference =
        inherit TypeReference
        abstract target: TupleType with get, set

    type [<AllowNullLiteral>] UnionOrIntersectionType =
        inherit Type
        abstract types: Type[] with get, set

    type [<AllowNullLiteral>] UnionType =
        inherit UnionOrIntersectionType

    type [<AllowNullLiteral>] IntersectionType =
        inherit UnionOrIntersectionType

    type StructuredType =
        U3<ObjectType, UnionType, IntersectionType>

    type [<AllowNullLiteral>] EvolvingArrayType =
        inherit ObjectType
        abstract elementType: Type with get, set
        abstract finalArrayType: Type option with get, set

    type [<AllowNullLiteral>] InstantiableType =
        inherit Type

    type [<AllowNullLiteral>] TypeParameter =
        inherit InstantiableType

    type [<AllowNullLiteral>] IndexedAccessType =
        inherit InstantiableType
        abstract objectType: Type with get, set
        abstract indexType: Type with get, set
        abstract ``constraint``: Type option with get, set
        abstract simplifiedForReading: Type option with get, set
        abstract simplifiedForWriting: Type option with get, set

    type TypeVariable =
        U2<TypeParameter, IndexedAccessType>

    type [<AllowNullLiteral>] IndexType =
        inherit InstantiableType
        abstract ``type``: U2<InstantiableType, UnionOrIntersectionType> with get, set

    type [<AllowNullLiteral>] ConditionalRoot =
        abstract node: ConditionalTypeNode with get, set
        abstract checkType: Type with get, set
        abstract extendsType: Type with get, set
        abstract isDistributive: bool with get, set
        abstract inferTypeParameters: TypeParameter[] option with get, set
        abstract outerTypeParameters: TypeParameter[] option with get, set
        abstract instantiations: Map<Type> option with get, set
        abstract aliasSymbol: Symbol option with get, set
        abstract aliasTypeArguments: Type[] option with get, set

    type [<AllowNullLiteral>] ConditionalType =
        inherit InstantiableType
        abstract root: ConditionalRoot with get, set
        abstract checkType: Type with get, set
        abstract extendsType: Type with get, set
        abstract resolvedTrueType: Type option with get, set
        abstract resolvedFalseType: Type option with get, set

    type [<AllowNullLiteral>] TemplateLiteralType =
        inherit InstantiableType
        abstract texts: string[] with get, set
        abstract types: Type[] with get, set

    type [<AllowNullLiteral>] StringMappingType =
        inherit InstantiableType
        abstract symbol: Symbol with get, set
        abstract ``type``: Type with get, set

    type [<AllowNullLiteral>] SubstitutionType =
        inherit InstantiableType
        abstract objectFlags: ObjectFlags with get, set
        abstract baseType: Type with get, set
        abstract substitute: Type with get, set

    type [<RequireQualifiedAccess>] SignatureKind =
        | Call = 0
        | Construct = 1

    type [<AllowNullLiteral>] Signature =
        abstract declaration: U2<SignatureDeclaration, JSDocSignature> option with get, set
        abstract typeParameters: TypeParameter[] option with get, set
        abstract parameters: Symbol[] with get, set
        abstract getDeclaration: unit -> SignatureDeclaration
        abstract getTypeParameters: unit -> TypeParameter[] option
        abstract getParameters: unit -> Symbol[]
        abstract getTypeParameterAtPosition: pos: float -> Type
        abstract getReturnType: unit -> Type
        abstract getDocumentationComment: typeChecker: TypeChecker option -> SymbolDisplayPart[]
        abstract getJsDocTags: unit -> JSDocTagInfo[]

    type [<RequireQualifiedAccess>] IndexKind =
        | String = 0
        | Number = 1

    type [<AllowNullLiteral>] IndexInfo =
        abstract keyType: Type with get, set
        abstract ``type``: Type with get, set
        abstract isReadonly: bool with get, set
        abstract declaration: IndexSignatureDeclaration option with get, set

    type [<RequireQualifiedAccess>] InferencePriority =
        | NakedTypeVariable = 1
        | SpeculativeTuple = 2
        | SubstituteSource = 4
        | HomomorphicMappedType = 8
        | PartialHomomorphicMappedType = 16
        | MappedTypeConstraint = 32
        | ContravariantConditional = 64
        | ReturnType = 128
        | LiteralKeyof = 256
        | NoConstraints = 512
        | AlwaysStrict = 1024
        | MaxValue = 2048
        | PriorityImpliesCombination = 416
        | Circularity = -1

    [<Obsolete("Use FileExtensionInfo instead.")>]
    type JsFileExtensionInfo =
        FileExtensionInfo

    type [<AllowNullLiteral>] FileExtensionInfo =
        abstract extension: string with get, set
        abstract isMixedContent: bool with get, set
        abstract scriptKind: ScriptKind option with get, set

    type [<AllowNullLiteral>] DiagnosticMessage =
        abstract key: string with get, set
        abstract category: DiagnosticCategory with get, set
        abstract code: float with get, set
        abstract message: string with get, set
        abstract reportsUnnecessary: DiagnosticMessageReportsUnnecessary option with get, set
        abstract reportsDeprecated: DiagnosticMessageReportsUnnecessary option with get, set

    /// A linked list of formatted diagnostic messages to be used as part of a multiline message.
    /// It is built from the bottom up, leaving the head to be the "main" diagnostic.
    /// While it seems that DiagnosticMessageChain is structurally similar to DiagnosticMessage,
    /// the difference is that messages are all preformatted in DMC.
    type [<AllowNullLiteral>] DiagnosticMessageChain =
        abstract messageText: string with get, set
        abstract category: DiagnosticCategory with get, set
        abstract code: float with get, set
        abstract next: DiagnosticMessageChain[] option with get, set

    type [<AllowNullLiteral>] Diagnostic =
        inherit DiagnosticRelatedInformation
        /// <summary>May store more in future. For now, this will simply be <c>true</c> to indicate when a diagnostic is an unused-identifier diagnostic.</summary>
        abstract reportsUnnecessary: DiagnosticMessageReportsUnnecessary option with get, set
        abstract reportsDeprecated: DiagnosticMessageReportsUnnecessary option with get, set
        abstract source: string option with get, set
        abstract relatedInformation: DiagnosticRelatedInformation[] option with get, set

    type [<AllowNullLiteral>] DiagnosticRelatedInformation =
        abstract category: DiagnosticCategory with get, set
        abstract code: float with get, set
        abstract file: SourceFile option with get, set
        abstract start: float option with get, set
        abstract length: float option with get, set
        abstract messageText: U2<string, DiagnosticMessageChain> with get, set

    type [<AllowNullLiteral>] DiagnosticWithLocation =
        inherit Diagnostic
        abstract file: SourceFile with get, set
        abstract start: float with get, set
        abstract length: float with get, set

    type [<RequireQualifiedAccess>] DiagnosticCategory =
        | Warning = 0
        | Error = 1
        | Suggestion = 2
        | Message = 3

    type [<RequireQualifiedAccess>] ModuleResolutionKind =
        | Classic = 1
        | NodeJs = 2
        | Node12 = 3
        | NodeNext = 99

    type [<AllowNullLiteral>] PluginImport =
        abstract name: string with get, set

    type [<AllowNullLiteral>] ProjectReference =
        /// A normalized path on disk
        abstract path: string with get, set
        /// The path as the user originally wrote it
        abstract originalPath: string option with get, set
        /// True if the output of this reference should be prepended to the output of this project. Only valid for --outFile compilations
        abstract prepend: bool option with get, set
        /// True if it is intended that this reference form a circularity
        abstract circular: bool option with get, set

    type [<RequireQualifiedAccess>] WatchFileKind =
        | FixedPollingInterval = 0
        | PriorityPollingInterval = 1
        | DynamicPriorityPolling = 2
        | FixedChunkSizePolling = 3
        | UseFsEvents = 4
        | UseFsEventsOnParentDirectory = 5

    type [<RequireQualifiedAccess>] WatchDirectoryKind =
        | UseFsEvents = 0
        | FixedPollingInterval = 1
        | DynamicPriorityPolling = 2
        | FixedChunkSizePolling = 3

    type [<RequireQualifiedAccess>] PollingWatchKind =
        | FixedInterval = 0
        | PriorityInterval = 1
        | DynamicPriority = 2
        | FixedChunkSize = 3

    type CompilerOptionsValue =
        U8<string, float, bool, U2<string, float>[], string[], MapLike<string[]>, PluginImport[], ProjectReference[]> option

    type [<AllowNullLiteral>] CompilerOptions =
        abstract allowJs: bool option with get, set
        abstract allowSyntheticDefaultImports: bool option with get, set
        abstract allowUmdGlobalAccess: bool option with get, set
        abstract allowUnreachableCode: bool option with get, set
        abstract allowUnusedLabels: bool option with get, set
        abstract alwaysStrict: bool option with get, set
        abstract baseUrl: string option with get, set
        abstract charset: string option with get, set
        abstract checkJs: bool option with get, set
        abstract declaration: bool option with get, set
        abstract declarationMap: bool option with get, set
        abstract emitDeclarationOnly: bool option with get, set
        abstract declarationDir: string option with get, set
        abstract disableSizeLimit: bool option with get, set
        abstract disableSourceOfProjectReferenceRedirect: bool option with get, set
        abstract disableSolutionSearching: bool option with get, set
        abstract disableReferencedProjectLoad: bool option with get, set
        abstract downlevelIteration: bool option with get, set
        abstract emitBOM: bool option with get, set
        abstract emitDecoratorMetadata: bool option with get, set
        abstract exactOptionalPropertyTypes: bool option with get, set
        abstract experimentalDecorators: bool option with get, set
        abstract forceConsistentCasingInFileNames: bool option with get, set
        abstract importHelpers: bool option with get, set
        abstract importsNotUsedAsValues: ImportsNotUsedAsValues option with get, set
        abstract inlineSourceMap: bool option with get, set
        abstract inlineSources: bool option with get, set
        abstract isolatedModules: bool option with get, set
        abstract jsx: JsxEmit option with get, set
        abstract keyofStringsOnly: bool option with get, set
        abstract lib: string[] option with get, set
        abstract locale: string option with get, set
        abstract mapRoot: string option with get, set
        abstract maxNodeModuleJsDepth: float option with get, set
        abstract ``module``: ModuleKind option with get, set
        abstract moduleResolution: ModuleResolutionKind option with get, set
        abstract newLine: NewLineKind option with get, set
        abstract noEmit: bool option with get, set
        abstract noEmitHelpers: bool option with get, set
        abstract noEmitOnError: bool option with get, set
        abstract noErrorTruncation: bool option with get, set
        abstract noFallthroughCasesInSwitch: bool option with get, set
        abstract noImplicitAny: bool option with get, set
        abstract noImplicitReturns: bool option with get, set
        abstract noImplicitThis: bool option with get, set
        abstract noStrictGenericChecks: bool option with get, set
        abstract noUnusedLocals: bool option with get, set
        abstract noUnusedParameters: bool option with get, set
        abstract noImplicitUseStrict: bool option with get, set
        abstract noPropertyAccessFromIndexSignature: bool option with get, set
        abstract assumeChangesOnlyAffectDirectDependencies: bool option with get, set
        abstract noLib: bool option with get, set
        abstract noResolve: bool option with get, set
        abstract noUncheckedIndexedAccess: bool option with get, set
        abstract out: string option with get, set
        abstract outDir: string option with get, set
        abstract outFile: string option with get, set
        abstract paths: MapLike<string[]> option with get, set
        abstract preserveConstEnums: bool option with get, set
        abstract noImplicitOverride: bool option with get, set
        abstract preserveSymlinks: bool option with get, set
        abstract preserveValueImports: bool option with get, set
        abstract project: string option with get, set
        abstract reactNamespace: string option with get, set
        abstract jsxFactory: string option with get, set
        abstract jsxFragmentFactory: string option with get, set
        abstract jsxImportSource: string option with get, set
        abstract composite: bool option with get, set
        abstract incremental: bool option with get, set
        abstract tsBuildInfoFile: string option with get, set
        abstract removeComments: bool option with get, set
        abstract rootDir: string option with get, set
        abstract rootDirs: string[] option with get, set
        abstract skipLibCheck: bool option with get, set
        abstract skipDefaultLibCheck: bool option with get, set
        abstract sourceMap: bool option with get, set
        abstract sourceRoot: string option with get, set
        abstract strict: bool option with get, set
        abstract strictFunctionTypes: bool option with get, set
        abstract strictBindCallApply: bool option with get, set
        abstract strictNullChecks: bool option with get, set
        abstract strictPropertyInitialization: bool option with get, set
        abstract stripInternal: bool option with get, set
        abstract suppressExcessPropertyErrors: bool option with get, set
        abstract suppressImplicitAnyIndexErrors: bool option with get, set
        abstract target: ScriptTarget option with get, set
        abstract traceResolution: bool option with get, set
        abstract useUnknownInCatchVariables: bool option with get, set
        abstract resolveJsonModule: bool option with get, set
        abstract types: string[] option with get, set
        /// Paths used to compute primary types search locations
        abstract typeRoots: string[] option with get, set
        abstract esModuleInterop: bool option with get, set
        abstract useDefineForClassFields: bool option with get, set
        [<EmitIndexer>] abstract Item: option: string -> U2<CompilerOptionsValue, TsConfigSourceFile> option with get, set

    type [<AllowNullLiteral>] WatchOptions =
        abstract watchFile: WatchFileKind option with get, set
        abstract watchDirectory: WatchDirectoryKind option with get, set
        abstract fallbackPolling: PollingWatchKind option with get, set
        abstract synchronousWatchDirectory: bool option with get, set
        abstract excludeDirectories: string[] option with get, set
        abstract excludeFiles: string[] option with get, set
        [<EmitIndexer>] abstract Item: option: string -> CompilerOptionsValue option with get, set

    type [<AllowNullLiteral>] TypeAcquisition =
        [<Obsolete("typingOptions.enableAutoDiscovery
Use typeAcquisition.enable instead.")>]
        abstract enableAutoDiscovery: bool option with get, set
        abstract enable: bool option with get, set
        abstract ``include``: string[] option with get, set
        abstract exclude: string[] option with get, set
        abstract disableFilenameBasedTypeAcquisition: bool option with get, set
        [<EmitIndexer>] abstract Item: option: string -> CompilerOptionsValue option with get, set

    type [<RequireQualifiedAccess>] ModuleKind =
        | None = 0
        | CommonJS = 1
        | AMD = 2
        | UMD = 3
        | System = 4
        | ES2015 = 5
        | ES2020 = 6
        | ES2022 = 7
        | ESNext = 99
        | Node12 = 100
        | NodeNext = 199

    type [<RequireQualifiedAccess>] JsxEmit =
        | None = 0
        | Preserve = 1
        | React = 2
        | ReactNative = 3
        | ReactJSX = 4
        | ReactJSXDev = 5

    type [<RequireQualifiedAccess>] ImportsNotUsedAsValues =
        | Remove = 0
        | Preserve = 1
        | Error = 2

    type [<RequireQualifiedAccess>] NewLineKind =
        | CarriageReturnLineFeed = 0
        | LineFeed = 1

    type [<AllowNullLiteral>] LineAndCharacter =
        /// 0-based.
        abstract line: float with get, set
        abstract character: float with get, set

    type [<RequireQualifiedAccess>] ScriptKind =
        | Unknown = 0
        | JS = 1
        | JSX = 2
        | TS = 3
        | TSX = 4
        | External = 5
        | JSON = 6
        /// Used on extensions that doesn't define the ScriptKind but the content defines it.
        /// Deferred extensions are going to be included in all project contexts.
        | Deferred = 7

    type [<RequireQualifiedAccess>] ScriptTarget =
        | ES3 = 0
        | ES5 = 1
        | ES2015 = 2
        | ES2016 = 3
        | ES2017 = 4
        | ES2018 = 5
        | ES2019 = 6
        | ES2020 = 7
        | ES2021 = 8
        | ES2022 = 9
        | ESNext = 99
        | JSON = 100
        | Latest = 99

    type [<RequireQualifiedAccess>] LanguageVariant =
        | Standard = 0
        | JSX = 1

    /// Either a parsed command line or a parsed tsconfig.json
    type [<AllowNullLiteral>] ParsedCommandLine =
        abstract options: CompilerOptions with get, set
        abstract typeAcquisition: TypeAcquisition option with get, set
        abstract fileNames: string[] with get, set
        abstract projectReferences: ProjectReference[] option with get, set
        abstract watchOptions: WatchOptions option with get, set
        abstract raw: obj option with get, set
        abstract errors: Diagnostic[] with get, set
        abstract wildcardDirectories: MapLike<WatchDirectoryFlags> option with get, set
        abstract compileOnSave: bool option with get, set

    type [<RequireQualifiedAccess>] WatchDirectoryFlags =
        | None = 0
        | Recursive = 1

    type [<AllowNullLiteral>] CreateProgramOptions =
        abstract rootNames: string[] with get, set
        abstract options: CompilerOptions with get, set
        abstract projectReferences: ProjectReference[] option with get, set
        abstract host: CompilerHost option with get, set
        abstract oldProgram: Program option with get, set
        abstract configFileParsingDiagnostics: Diagnostic[] option with get, set

    type [<AllowNullLiteral>] ModuleResolutionHost =
        abstract fileExists: fileName: string -> bool
        abstract readFile: fileName: string -> string option
        abstract trace: s: string -> unit
        abstract directoryExists: directoryName: string -> bool
        /// <summary>Resolve a symbolic link.</summary>
        /// <seealso href="https://nodejs.org/api/fs.html#fs_fs_realpathsync_path_options" />
        abstract realpath: path: string -> string
        abstract getCurrentDirectory: unit -> string
        abstract getDirectories: path: string -> string[]
        abstract useCaseSensitiveFileNames: U2<bool, (unit -> bool)> option with get, set

    /// <summary>
    /// Represents the result of module resolution.
    /// Module resolution will pick up tsx/jsx/js files even if '--jsx' and '--allowJs' are turned off.
    /// The Program will then filter results based on these flags.
    ///
    /// Prefer to return a <c>ResolvedModuleFull</c> so that the file type does not have to be inferred.
    /// </summary>
    type [<AllowNullLiteral>] ResolvedModule =
        /// Path of the file the module was resolved to.
        abstract resolvedFileName: string with get, set
        /// <summary>True if <c>resolvedFileName</c> comes from <c>node_modules</c>.</summary>
        abstract isExternalLibraryImport: bool option with get, set

    /// <summary>
    /// ResolvedModule with an explicitly provided <c>extension</c> property.
    /// Prefer this over <c>ResolvedModule</c>.
    /// If changing this, remember to change <c>moduleResolutionIsEqualTo</c>.
    /// </summary>
    type [<AllowNullLiteral>] ResolvedModuleFull =
        inherit ResolvedModule
        /// Extension of resolvedFileName. This must match what's at the end of resolvedFileName.
        /// This is optional for backwards-compatibility, but will be added if not provided.
        abstract extension: Extension with get, set
        abstract packageId: PackageId option with get, set

    /// <summary>
    /// Unique identifier with a package name and version.
    /// If changing this, remember to change <c>packageIdIsEqual</c>.
    /// </summary>
    type [<AllowNullLiteral>] PackageId =
        /// <summary>
        /// Name of the package.
        /// Should not include <c>@types</c>.
        /// If accessing a non-index file, this should include its name e.g. "foo/bar".
        /// </summary>
        abstract name: string with get, set
        /// Name of a submodule within this package.
        /// May be "".
        abstract subModuleName: string with get, set
        /// Version of the package, e.g. "1.2.3"
        abstract version: string with get, set

    type [<StringEnum>] [<RequireQualifiedAccess>] Extension =
        | [<CompiledName ".ts">] Ts
        | [<CompiledName ".tsx">] Tsx
        | [<CompiledName ".d.ts">] Dts
        | [<CompiledName ".js">] Js
        | [<CompiledName ".jsx">] Jsx
        | [<CompiledName ".json">] Json
        | [<CompiledName ".tsbuildinfo">] TsBuildInfo
        | [<CompiledName ".mjs">] Mjs
        | [<CompiledName ".mts">] Mts
        | [<CompiledName ".d.mts">] Dmts
        | [<CompiledName ".cjs">] Cjs
        | [<CompiledName ".cts">] Cts
        | [<CompiledName ".d.cts">] Dcts

    type [<AllowNullLiteral>] ResolvedModuleWithFailedLookupLocations =
        abstract resolvedModule: ResolvedModuleFull option

    type [<AllowNullLiteral>] ResolvedTypeReferenceDirective =
        abstract primary: bool with get, set
        abstract resolvedFileName: string option with get, set
        abstract packageId: PackageId option with get, set
        /// <summary>True if <c>resolvedFileName</c> comes from <c>node_modules</c>.</summary>
        abstract isExternalLibraryImport: bool option with get, set

    type [<AllowNullLiteral>] ResolvedTypeReferenceDirectiveWithFailedLookupLocations =
        abstract resolvedTypeReferenceDirective: ResolvedTypeReferenceDirective option
        abstract failedLookupLocations: string[]

    type [<AllowNullLiteral>] CompilerHost =
        inherit ModuleResolutionHost
        abstract getSourceFile: fileName: string * languageVersion: ScriptTarget * ?onError: (string -> unit) * ?shouldCreateNewSourceFile: bool -> SourceFile option
        abstract getSourceFileByPath: fileName: string * path: Path * languageVersion: ScriptTarget * ?onError: (string -> unit) * ?shouldCreateNewSourceFile: bool -> SourceFile option
        abstract getCancellationToken: unit -> CancellationToken
        abstract getDefaultLibFileName: options: CompilerOptions -> string
        abstract getDefaultLibLocation: unit -> string
        abstract writeFile: WriteFileCallback with get, set
        abstract getCurrentDirectory: unit -> string
        abstract getCanonicalFileName: fileName: string -> string
        abstract useCaseSensitiveFileNames: unit -> bool
        abstract getNewLine: unit -> string
        abstract readDirectory: rootDir: string * extensions: string[] * excludes: string[] option * includes: string[] * ?depth: float -> string[]
        abstract resolveModuleNames: moduleNames: string[] * containingFile: string * reusedNames: string[] option * redirectedReference: ResolvedProjectReference option * options: CompilerOptions * ?containingSourceFile: SourceFile -> ResolvedModule option[]
        /// <summary>Returns the module resolution cache used by a provided <c>resolveModuleNames</c> implementation so that any non-name module resolution operations (eg, package.json lookup) can reuse it</summary>
        abstract getModuleResolutionCache: unit -> ModuleResolutionCache option
        /// This method is a companion for 'resolveModuleNames' and is used to resolve 'types' references to actual type declaration files
        abstract resolveTypeReferenceDirectives: typeReferenceDirectiveNames: string[] * containingFile: string * redirectedReference: ResolvedProjectReference option * options: CompilerOptions -> ResolvedTypeReferenceDirective option[]
        abstract getEnvironmentVariable: name: string -> string option
        abstract createHash: data: string -> string
        abstract getParsedCommandLine: fileName: string -> ParsedCommandLine option

    type [<AllowNullLiteral>] SourceMapRange =
        inherit TextRange
        abstract source: SourceMapSource option with get, set

    type [<AllowNullLiteral>] SourceMapSource =
        abstract fileName: string with get, set
        abstract text: string with get, set
        abstract skipTrivia: (float -> float) option with get, set
        abstract getLineAndCharacterOfPosition: pos: float -> LineAndCharacter

    type [<RequireQualifiedAccess>] EmitFlags =
        | None = 0
        | SingleLine = 1
        | AdviseOnEmitNode = 2
        | NoSubstitution = 4
        | CapturesThis = 8
        | NoLeadingSourceMap = 16
        | NoTrailingSourceMap = 32
        | NoSourceMap = 48
        | NoNestedSourceMaps = 64
        | NoTokenLeadingSourceMaps = 128
        | NoTokenTrailingSourceMaps = 256
        | NoTokenSourceMaps = 384
        | NoLeadingComments = 512
        | NoTrailingComments = 1024
        | NoComments = 1536
        | NoNestedComments = 2048
        | HelperName = 4096
        | ExportName = 8192
        | LocalName = 16384
        | InternalName = 32768
        | Indented = 65536
        | NoIndentation = 131072
        | AsyncFunctionBody = 262144
        | ReuseTempVariableScope = 524288
        | CustomPrologue = 1048576
        | NoHoisting = 2097152
        | HasEndOfDeclarationMarker = 4194304
        | Iterator = 8388608
        | NoAsciiEscaping = 16777216

    type [<AllowNullLiteral>] EmitHelperBase =
        abstract name: string
        abstract scoped: bool
        abstract text: U2<string, (EmitHelperUniqueNameCallback -> string)>
        abstract priority: float option
        abstract dependencies: EmitHelper[] option

    type [<AllowNullLiteral>] ScopedEmitHelper =
        inherit EmitHelperBase
        abstract scoped: bool

    type [<AllowNullLiteral>] UnscopedEmitHelper =
        inherit EmitHelperBase
        abstract scoped: bool
        abstract text: string

    type [<TypeScriptTaggedUnion("scoped")>] [<RequireQualifiedAccess>] EmitHelper =
        | [<CompiledValue false>] UnscopedEmitHelper of UnscopedEmitHelper
        | [<CompiledValue true>] ScopedEmitHelper of ScopedEmitHelper

    type [<AllowNullLiteral>] EmitHelperUniqueNameCallback =
        [<Emit "$0($1...)">] abstract Invoke: name: string -> string

    type [<RequireQualifiedAccess>] EmitHint =
        | SourceFile = 0
        | Expression = 1
        | IdentifierName = 2
        | MappedTypeParameter = 3
        | Unspecified = 4
        | EmbeddedStatement = 5
        | JsxAttributeValue = 6

    type [<RequireQualifiedAccess>] OuterExpressionKinds =
        | Parentheses = 1
        | TypeAssertions = 2
        | NonNullAssertions = 4
        | PartiallyEmittedExpressions = 8
        | Assertions = 6
        | All = 15
        | ExcludeJSDocTypeAssertion = 16

    type [<StringEnum>] [<RequireQualifiedAccess>] TypeOfTag =
        | Undefined
        | Number
        | Bigint
        | Boolean
        | String
        | Symbol
        | Object
        | Function

    type [<AllowNullLiteral>] NodeFactory =
        abstract createNodeArray: ?elements: 'T[] * ?hasTrailingComma: bool -> 'T[]
        abstract createNumericLiteral: value: U2<string, float> * ?numericLiteralFlags: TokenFlags -> NumericLiteral
        abstract createBigIntLiteral: value: U2<string, PseudoBigInt> -> BigIntLiteral
        abstract createStringLiteral: text: string * ?isSingleQuote: bool -> StringLiteral
        abstract createStringLiteralFromNode: sourceNode: PropertyNameLiteral * ?isSingleQuote: bool -> StringLiteral
        abstract createRegularExpressionLiteral: text: string -> RegularExpressionLiteral
        abstract createIdentifier: text: string -> Identifier
        /// <summary>Create a unique temporary variable.</summary>
        /// <param name="recordTempVariable">
        /// An optional callback used to record the temporary variable name. This
        /// should usually be a reference to <c>hoistVariableDeclaration</c> from a <c>TransformationContext</c>, but
        /// can be <c>undefined</c> if you plan to record the temporary variable manually.
        /// </param>
        /// <param name="reservedInNestedScopes">
        /// When <c>true</c>, reserves the temporary variable name in all nested scopes
        /// during emit so that the variable can be referenced in a nested function body. This is an alternative to
        /// setting <c>EmitFlags.ReuseTempVariableScope</c> on the nested function itself.
        /// </param>
        abstract createTempVariable: recordTempVariable: (Identifier -> unit) option * ?reservedInNestedScopes: bool -> Identifier
        /// <summary>Create a unique temporary variable for use in a loop.</summary>
        /// <param name="reservedInNestedScopes">
        /// When <c>true</c>, reserves the temporary variable name in all nested scopes
        /// during emit so that the variable can be referenced in a nested function body. This is an alternative to
        /// setting <c>EmitFlags.ReuseTempVariableScope</c> on the nested function itself.
        /// </param>
        abstract createLoopVariable: ?reservedInNestedScopes: bool -> Identifier
        /// Create a unique name based on the supplied text.
        abstract createUniqueName: text: string * ?flags: GeneratedIdentifierFlags -> Identifier
        /// Create a unique name generated for a node.
        abstract getGeneratedNameForNode: node: Node option * ?flags: GeneratedIdentifierFlags -> Identifier
        abstract createPrivateIdentifier: text: string -> PrivateIdentifier
        abstract createToken: token: SyntaxKind -> SuperExpression
        abstract createToken: token: 'TKind -> Token<'TKind>
        abstract createSuper: unit -> SuperExpression
        abstract createThis: unit -> ThisExpression
        abstract createNull: unit -> NullLiteral
        abstract createTrue: unit -> TrueLiteral
        abstract createFalse: unit -> FalseLiteral
        abstract createModifier: kind: 'T -> ModifierToken<'T>
        abstract createModifiersFromModifierFlags: flags: ModifierFlags -> Modifier[] option
        abstract createQualifiedName: left: EntityName * right: U2<string, Identifier> -> QualifiedName
        abstract updateQualifiedName: node: QualifiedName * left: EntityName * right: Identifier -> QualifiedName
        abstract createComputedPropertyName: expression: Expression -> ComputedPropertyName
        abstract updateComputedPropertyName: node: ComputedPropertyName * expression: Expression -> ComputedPropertyName
        abstract createTypeParameterDeclaration: name: U2<string, Identifier> * ?``constraint``: TypeNode * ?defaultType: TypeNode -> TypeParameterDeclaration
        abstract updateTypeParameterDeclaration: node: TypeParameterDeclaration * name: Identifier * ``constraint``: TypeNode option * defaultType: TypeNode option -> TypeParameterDeclaration
        abstract createParameterDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * dotDotDotToken: DotDotDotToken option * name: U2<string, BindingName> * ?questionToken: QuestionToken * ?``type``: TypeNode * ?initializer: Expression -> ParameterDeclaration
        abstract updateParameterDeclaration: node: ParameterDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * dotDotDotToken: DotDotDotToken option * name: U2<string, BindingName> * questionToken: QuestionToken option * ``type``: TypeNode option * initializer: Expression option -> ParameterDeclaration
        abstract createDecorator: expression: Expression -> Decorator
        abstract updateDecorator: node: Decorator * expression: Expression -> Decorator
        abstract createPropertySignature: modifiers: Modifier[] option * name: U2<PropertyName, string> * questionToken: QuestionToken option * ``type``: TypeNode option -> PropertySignature
        abstract updatePropertySignature: node: PropertySignature * modifiers: Modifier[] option * name: PropertyName * questionToken: QuestionToken option * ``type``: TypeNode option -> PropertySignature
        abstract createPropertyDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * name: U2<string, PropertyName> * questionOrExclamationToken: U2<QuestionToken, ExclamationToken> option * ``type``: TypeNode option * initializer: Expression option -> PropertyDeclaration
        abstract updatePropertyDeclaration: node: PropertyDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * name: U2<string, PropertyName> * questionOrExclamationToken: U2<QuestionToken, ExclamationToken> option * ``type``: TypeNode option * initializer: Expression option -> PropertyDeclaration
        abstract createMethodSignature: modifiers: Modifier[] option * name: U2<string, PropertyName> * questionToken: QuestionToken option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option -> MethodSignature
        abstract updateMethodSignature: node: MethodSignature * modifiers: Modifier[] option * name: PropertyName * questionToken: QuestionToken option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option -> MethodSignature
        abstract createMethodDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * asteriskToken: AsteriskToken option * name: U2<string, PropertyName> * questionToken: QuestionToken option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option * body: Block option -> MethodDeclaration
        abstract updateMethodDeclaration: node: MethodDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * asteriskToken: AsteriskToken option * name: PropertyName * questionToken: QuestionToken option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option * body: Block option -> MethodDeclaration
        abstract createConstructorDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * parameters: ParameterDeclaration[] * body: Block option -> ConstructorDeclaration
        abstract updateConstructorDeclaration: node: ConstructorDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * parameters: ParameterDeclaration[] * body: Block option -> ConstructorDeclaration
        abstract createGetAccessorDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * name: U2<string, PropertyName> * parameters: ParameterDeclaration[] * ``type``: TypeNode option * body: Block option -> GetAccessorDeclaration
        abstract updateGetAccessorDeclaration: node: GetAccessorDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * name: PropertyName * parameters: ParameterDeclaration[] * ``type``: TypeNode option * body: Block option -> GetAccessorDeclaration
        abstract createSetAccessorDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * name: U2<string, PropertyName> * parameters: ParameterDeclaration[] * body: Block option -> SetAccessorDeclaration
        abstract updateSetAccessorDeclaration: node: SetAccessorDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * name: PropertyName * parameters: ParameterDeclaration[] * body: Block option -> SetAccessorDeclaration
        abstract createCallSignature: typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option -> CallSignatureDeclaration
        abstract updateCallSignature: node: CallSignatureDeclaration * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option -> CallSignatureDeclaration
        abstract createConstructSignature: typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option -> ConstructSignatureDeclaration
        abstract updateConstructSignature: node: ConstructSignatureDeclaration * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option -> ConstructSignatureDeclaration
        abstract createIndexSignature: decorators: Decorator[] option * modifiers: Modifier[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode -> IndexSignatureDeclaration
        abstract updateIndexSignature: node: IndexSignatureDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode -> IndexSignatureDeclaration
        abstract createTemplateLiteralTypeSpan: ``type``: TypeNode * literal: U2<TemplateMiddle, TemplateTail> -> TemplateLiteralTypeSpan
        abstract updateTemplateLiteralTypeSpan: node: TemplateLiteralTypeSpan * ``type``: TypeNode * literal: U2<TemplateMiddle, TemplateTail> -> TemplateLiteralTypeSpan
        abstract createClassStaticBlockDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * body: Block -> ClassStaticBlockDeclaration
        abstract updateClassStaticBlockDeclaration: node: ClassStaticBlockDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * body: Block -> ClassStaticBlockDeclaration
        abstract createKeywordTypeNode: kind: 'TKind -> KeywordTypeNode<'TKind>
        abstract createTypePredicateNode: assertsModifier: AssertsKeyword option * parameterName: U3<Identifier, ThisTypeNode, string> * ``type``: TypeNode option -> TypePredicateNode
        abstract updateTypePredicateNode: node: TypePredicateNode * assertsModifier: AssertsKeyword option * parameterName: U2<Identifier, ThisTypeNode> * ``type``: TypeNode option -> TypePredicateNode
        abstract createTypeReferenceNode: typeName: U2<string, EntityName> * ?typeArguments: TypeNode[] -> TypeReferenceNode
        abstract updateTypeReferenceNode: node: TypeReferenceNode * typeName: EntityName * typeArguments: TypeNode[] option -> TypeReferenceNode
        abstract createFunctionTypeNode: typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode -> FunctionTypeNode
        abstract updateFunctionTypeNode: node: FunctionTypeNode * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode -> FunctionTypeNode
        abstract createConstructorTypeNode: modifiers: Modifier[] option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode -> ConstructorTypeNode
        [<Obsolete("")>]
        abstract createConstructorTypeNode: typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode -> ConstructorTypeNode
        abstract updateConstructorTypeNode: node: ConstructorTypeNode * modifiers: Modifier[] option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode -> ConstructorTypeNode
        [<Obsolete("")>]
        abstract updateConstructorTypeNode: node: ConstructorTypeNode * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode -> ConstructorTypeNode
        abstract createTypeQueryNode: exprName: EntityName -> TypeQueryNode
        abstract updateTypeQueryNode: node: TypeQueryNode * exprName: EntityName -> TypeQueryNode
        abstract createTypeLiteralNode: members: TypeElement[] option -> TypeLiteralNode
        abstract updateTypeLiteralNode: node: TypeLiteralNode * members: TypeElement[] -> TypeLiteralNode
        abstract createArrayTypeNode: elementType: TypeNode -> ArrayTypeNode
        abstract updateArrayTypeNode: node: ArrayTypeNode * elementType: TypeNode -> ArrayTypeNode
        abstract createTupleTypeNode: elements: U2<TypeNode, NamedTupleMember>[] -> TupleTypeNode
        abstract updateTupleTypeNode: node: TupleTypeNode * elements: U2<TypeNode, NamedTupleMember>[] -> TupleTypeNode
        abstract createNamedTupleMember: dotDotDotToken: DotDotDotToken option * name: Identifier * questionToken: QuestionToken option * ``type``: TypeNode -> NamedTupleMember
        abstract updateNamedTupleMember: node: NamedTupleMember * dotDotDotToken: DotDotDotToken option * name: Identifier * questionToken: QuestionToken option * ``type``: TypeNode -> NamedTupleMember
        abstract createOptionalTypeNode: ``type``: TypeNode -> OptionalTypeNode
        abstract updateOptionalTypeNode: node: OptionalTypeNode * ``type``: TypeNode -> OptionalTypeNode
        abstract createRestTypeNode: ``type``: TypeNode -> RestTypeNode
        abstract updateRestTypeNode: node: RestTypeNode * ``type``: TypeNode -> RestTypeNode
        abstract createUnionTypeNode: types: TypeNode[] -> UnionTypeNode
        abstract updateUnionTypeNode: node: UnionTypeNode * types: TypeNode[] -> UnionTypeNode
        abstract createIntersectionTypeNode: types: TypeNode[] -> IntersectionTypeNode
        abstract updateIntersectionTypeNode: node: IntersectionTypeNode * types: TypeNode[] -> IntersectionTypeNode
        abstract createConditionalTypeNode: checkType: TypeNode * extendsType: TypeNode * trueType: TypeNode * falseType: TypeNode -> ConditionalTypeNode
        abstract updateConditionalTypeNode: node: ConditionalTypeNode * checkType: TypeNode * extendsType: TypeNode * trueType: TypeNode * falseType: TypeNode -> ConditionalTypeNode
        abstract createInferTypeNode: typeParameter: TypeParameterDeclaration -> InferTypeNode
        abstract updateInferTypeNode: node: InferTypeNode * typeParameter: TypeParameterDeclaration -> InferTypeNode
        abstract createImportTypeNode: argument: TypeNode * ?qualifier: EntityName * ?typeArguments: TypeNode[] * ?isTypeOf: bool -> ImportTypeNode
        abstract updateImportTypeNode: node: ImportTypeNode * argument: TypeNode * qualifier: EntityName option * typeArguments: TypeNode[] option * ?isTypeOf: bool -> ImportTypeNode
        abstract createParenthesizedType: ``type``: TypeNode -> ParenthesizedTypeNode
        abstract updateParenthesizedType: node: ParenthesizedTypeNode * ``type``: TypeNode -> ParenthesizedTypeNode
        abstract createThisTypeNode: unit -> ThisTypeNode
        abstract createTypeOperatorNode: operator: SyntaxKind * ``type``: TypeNode -> TypeOperatorNode
        abstract updateTypeOperatorNode: node: TypeOperatorNode * ``type``: TypeNode -> TypeOperatorNode
        abstract createIndexedAccessTypeNode: objectType: TypeNode * indexType: TypeNode -> IndexedAccessTypeNode
        abstract updateIndexedAccessTypeNode: node: IndexedAccessTypeNode * objectType: TypeNode * indexType: TypeNode -> IndexedAccessTypeNode
        abstract createMappedTypeNode: readonlyToken: U3<ReadonlyKeyword, PlusToken, MinusToken> option * typeParameter: TypeParameterDeclaration * nameType: TypeNode option * questionToken: U3<QuestionToken, PlusToken, MinusToken> option * ``type``: TypeNode option * members: TypeElement[] option -> MappedTypeNode
        abstract updateMappedTypeNode: node: MappedTypeNode * readonlyToken: U3<ReadonlyKeyword, PlusToken, MinusToken> option * typeParameter: TypeParameterDeclaration * nameType: TypeNode option * questionToken: U3<QuestionToken, PlusToken, MinusToken> option * ``type``: TypeNode option * members: TypeElement[] option -> MappedTypeNode
        abstract createLiteralTypeNode: literal: obj -> LiteralTypeNode
        abstract updateLiteralTypeNode: node: LiteralTypeNode * literal: obj -> LiteralTypeNode
        abstract createTemplateLiteralType: head: TemplateHead * templateSpans: TemplateLiteralTypeSpan[] -> TemplateLiteralTypeNode
        abstract updateTemplateLiteralType: node: TemplateLiteralTypeNode * head: TemplateHead * templateSpans: TemplateLiteralTypeSpan[] -> TemplateLiteralTypeNode
        abstract createObjectBindingPattern: elements: BindingElement[] -> ObjectBindingPattern
        abstract updateObjectBindingPattern: node: ObjectBindingPattern * elements: BindingElement[] -> ObjectBindingPattern
        abstract createArrayBindingPattern: elements: ArrayBindingElement[] -> ArrayBindingPattern
        abstract updateArrayBindingPattern: node: ArrayBindingPattern * elements: ArrayBindingElement[] -> ArrayBindingPattern
        abstract createBindingElement: dotDotDotToken: DotDotDotToken option * propertyName: U2<string, PropertyName> option * name: U2<string, BindingName> * ?initializer: Expression -> BindingElement
        abstract updateBindingElement: node: BindingElement * dotDotDotToken: DotDotDotToken option * propertyName: PropertyName option * name: BindingName * initializer: Expression option -> BindingElement
        abstract createArrayLiteralExpression: ?elements: Expression[] * ?multiLine: bool -> ArrayLiteralExpression
        abstract updateArrayLiteralExpression: node: ArrayLiteralExpression * elements: Expression[] -> ArrayLiteralExpression
        abstract createObjectLiteralExpression: ?properties: ObjectLiteralElementLike[] * ?multiLine: bool -> ObjectLiteralExpression
        abstract updateObjectLiteralExpression: node: ObjectLiteralExpression * properties: ObjectLiteralElementLike[] -> ObjectLiteralExpression
        abstract createPropertyAccessExpression: expression: Expression * name: U2<string, MemberName> -> PropertyAccessExpression
        abstract updatePropertyAccessExpression: node: PropertyAccessExpression * expression: Expression * name: MemberName -> PropertyAccessExpression
        abstract createPropertyAccessChain: expression: Expression * questionDotToken: QuestionDotToken option * name: U2<string, MemberName> -> PropertyAccessChain
        abstract updatePropertyAccessChain: node: PropertyAccessChain * expression: Expression * questionDotToken: QuestionDotToken option * name: MemberName -> PropertyAccessChain
        abstract createElementAccessExpression: expression: Expression * index: U2<float, Expression> -> ElementAccessExpression
        abstract updateElementAccessExpression: node: ElementAccessExpression * expression: Expression * argumentExpression: Expression -> ElementAccessExpression
        abstract createElementAccessChain: expression: Expression * questionDotToken: QuestionDotToken option * index: U2<float, Expression> -> ElementAccessChain
        abstract updateElementAccessChain: node: ElementAccessChain * expression: Expression * questionDotToken: QuestionDotToken option * argumentExpression: Expression -> ElementAccessChain
        abstract createCallExpression: expression: Expression * typeArguments: TypeNode[] option * argumentsArray: Expression[] option -> CallExpression
        abstract updateCallExpression: node: CallExpression * expression: Expression * typeArguments: TypeNode[] option * argumentsArray: Expression[] -> CallExpression
        abstract createCallChain: expression: Expression * questionDotToken: QuestionDotToken option * typeArguments: TypeNode[] option * argumentsArray: Expression[] option -> CallChain
        abstract updateCallChain: node: CallChain * expression: Expression * questionDotToken: QuestionDotToken option * typeArguments: TypeNode[] option * argumentsArray: Expression[] -> CallChain
        abstract createNewExpression: expression: Expression * typeArguments: TypeNode[] option * argumentsArray: Expression[] option -> NewExpression
        abstract updateNewExpression: node: NewExpression * expression: Expression * typeArguments: TypeNode[] option * argumentsArray: Expression[] option -> NewExpression
        abstract createTaggedTemplateExpression: tag: Expression * typeArguments: TypeNode[] option * template: TemplateLiteral -> TaggedTemplateExpression
        abstract updateTaggedTemplateExpression: node: TaggedTemplateExpression * tag: Expression * typeArguments: TypeNode[] option * template: TemplateLiteral -> TaggedTemplateExpression
        abstract createTypeAssertion: ``type``: TypeNode * expression: Expression -> TypeAssertion
        abstract updateTypeAssertion: node: TypeAssertion * ``type``: TypeNode * expression: Expression -> TypeAssertion
        abstract createParenthesizedExpression: expression: Expression -> ParenthesizedExpression
        abstract updateParenthesizedExpression: node: ParenthesizedExpression * expression: Expression -> ParenthesizedExpression
        abstract createFunctionExpression: modifiers: Modifier[] option * asteriskToken: AsteriskToken option * name: U2<string, Identifier> option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] option * ``type``: TypeNode option * body: Block -> FunctionExpression
        abstract updateFunctionExpression: node: FunctionExpression * modifiers: Modifier[] option * asteriskToken: AsteriskToken option * name: Identifier option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option * body: Block -> FunctionExpression
        abstract createArrowFunction: modifiers: Modifier[] option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option * equalsGreaterThanToken: EqualsGreaterThanToken option * body: ConciseBody -> ArrowFunction
        abstract updateArrowFunction: node: ArrowFunction * modifiers: Modifier[] option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option * equalsGreaterThanToken: EqualsGreaterThanToken * body: ConciseBody -> ArrowFunction
        abstract createDeleteExpression: expression: Expression -> DeleteExpression
        abstract updateDeleteExpression: node: DeleteExpression * expression: Expression -> DeleteExpression
        abstract createTypeOfExpression: expression: Expression -> TypeOfExpression
        abstract updateTypeOfExpression: node: TypeOfExpression * expression: Expression -> TypeOfExpression
        abstract createVoidExpression: expression: Expression -> VoidExpression
        abstract updateVoidExpression: node: VoidExpression * expression: Expression -> VoidExpression
        abstract createAwaitExpression: expression: Expression -> AwaitExpression
        abstract updateAwaitExpression: node: AwaitExpression * expression: Expression -> AwaitExpression
        abstract createPrefixUnaryExpression: operator: PrefixUnaryOperator * operand: Expression -> PrefixUnaryExpression
        abstract updatePrefixUnaryExpression: node: PrefixUnaryExpression * operand: Expression -> PrefixUnaryExpression
        abstract createPostfixUnaryExpression: operand: Expression * operator: PostfixUnaryOperator -> PostfixUnaryExpression
        abstract updatePostfixUnaryExpression: node: PostfixUnaryExpression * operand: Expression -> PostfixUnaryExpression
        abstract createBinaryExpression: left: Expression * operator: U2<BinaryOperator, BinaryOperatorToken> * right: Expression -> BinaryExpression
        abstract updateBinaryExpression: node: BinaryExpression * left: Expression * operator: U2<BinaryOperator, BinaryOperatorToken> * right: Expression -> BinaryExpression
        abstract createConditionalExpression: condition: Expression * questionToken: QuestionToken option * whenTrue: Expression * colonToken: ColonToken option * whenFalse: Expression -> ConditionalExpression
        abstract updateConditionalExpression: node: ConditionalExpression * condition: Expression * questionToken: QuestionToken * whenTrue: Expression * colonToken: ColonToken * whenFalse: Expression -> ConditionalExpression
        abstract createTemplateExpression: head: TemplateHead * templateSpans: TemplateSpan[] -> TemplateExpression
        abstract updateTemplateExpression: node: TemplateExpression * head: TemplateHead * templateSpans: TemplateSpan[] -> TemplateExpression
        abstract createTemplateHead: text: string * ?rawText: string * ?templateFlags: TokenFlags -> TemplateHead
        abstract createTemplateHead: text: string option * rawText: string * ?templateFlags: TokenFlags -> TemplateHead
        abstract createTemplateMiddle: text: string * ?rawText: string * ?templateFlags: TokenFlags -> TemplateMiddle
        abstract createTemplateMiddle: text: string option * rawText: string * ?templateFlags: TokenFlags -> TemplateMiddle
        abstract createTemplateTail: text: string * ?rawText: string * ?templateFlags: TokenFlags -> TemplateTail
        abstract createTemplateTail: text: string option * rawText: string * ?templateFlags: TokenFlags -> TemplateTail
        abstract createNoSubstitutionTemplateLiteral: text: string * ?rawText: string -> NoSubstitutionTemplateLiteral
        abstract createNoSubstitutionTemplateLiteral: text: string option * rawText: string -> NoSubstitutionTemplateLiteral
        abstract createYieldExpression: asteriskToken: AsteriskToken * expression: Expression -> YieldExpression
        abstract createYieldExpression: asteriskToken: obj * expression: Expression option -> YieldExpression
        abstract updateYieldExpression: node: YieldExpression * asteriskToken: AsteriskToken option * expression: Expression option -> YieldExpression
        abstract createSpreadElement: expression: Expression -> SpreadElement
        abstract updateSpreadElement: node: SpreadElement * expression: Expression -> SpreadElement
        abstract createClassExpression: decorators: Decorator[] option * modifiers: Modifier[] option * name: U2<string, Identifier> option * typeParameters: TypeParameterDeclaration[] option * heritageClauses: HeritageClause[] option * members: ClassElement[] -> ClassExpression
        abstract updateClassExpression: node: ClassExpression * decorators: Decorator[] option * modifiers: Modifier[] option * name: Identifier option * typeParameters: TypeParameterDeclaration[] option * heritageClauses: HeritageClause[] option * members: ClassElement[] -> ClassExpression
        abstract createOmittedExpression: unit -> OmittedExpression
        abstract createExpressionWithTypeArguments: expression: Expression * typeArguments: TypeNode[] option -> ExpressionWithTypeArguments
        abstract updateExpressionWithTypeArguments: node: ExpressionWithTypeArguments * expression: Expression * typeArguments: TypeNode[] option -> ExpressionWithTypeArguments
        abstract createAsExpression: expression: Expression * ``type``: TypeNode -> AsExpression
        abstract updateAsExpression: node: AsExpression * expression: Expression * ``type``: TypeNode -> AsExpression
        abstract createNonNullExpression: expression: Expression -> NonNullExpression
        abstract updateNonNullExpression: node: NonNullExpression * expression: Expression -> NonNullExpression
        abstract createNonNullChain: expression: Expression -> NonNullChain
        abstract updateNonNullChain: node: NonNullChain * expression: Expression -> NonNullChain
        abstract createMetaProperty: keywordToken: obj * name: Identifier -> MetaProperty
        abstract updateMetaProperty: node: MetaProperty * name: Identifier -> MetaProperty
        abstract createTemplateSpan: expression: Expression * literal: U2<TemplateMiddle, TemplateTail> -> TemplateSpan
        abstract updateTemplateSpan: node: TemplateSpan * expression: Expression * literal: U2<TemplateMiddle, TemplateTail> -> TemplateSpan
        abstract createSemicolonClassElement: unit -> SemicolonClassElement
        abstract createBlock: statements: Statement[] * ?multiLine: bool -> Block
        abstract updateBlock: node: Block * statements: Statement[] -> Block
        abstract createVariableStatement: modifiers: Modifier[] option * declarationList: U2<VariableDeclarationList, VariableDeclaration[]> -> VariableStatement
        abstract updateVariableStatement: node: VariableStatement * modifiers: Modifier[] option * declarationList: VariableDeclarationList -> VariableStatement
        abstract createEmptyStatement: unit -> EmptyStatement
        abstract createExpressionStatement: expression: Expression -> ExpressionStatement
        abstract updateExpressionStatement: node: ExpressionStatement * expression: Expression -> ExpressionStatement
        abstract createIfStatement: expression: Expression * thenStatement: Statement * ?elseStatement: Statement -> IfStatement
        abstract updateIfStatement: node: IfStatement * expression: Expression * thenStatement: Statement * elseStatement: Statement option -> IfStatement
        abstract createDoStatement: statement: Statement * expression: Expression -> DoStatement
        abstract updateDoStatement: node: DoStatement * statement: Statement * expression: Expression -> DoStatement
        abstract createWhileStatement: expression: Expression * statement: Statement -> WhileStatement
        abstract updateWhileStatement: node: WhileStatement * expression: Expression * statement: Statement -> WhileStatement
        abstract createForStatement: initializer: ForInitializer option * condition: Expression option * incrementor: Expression option * statement: Statement -> ForStatement
        abstract updateForStatement: node: ForStatement * initializer: ForInitializer option * condition: Expression option * incrementor: Expression option * statement: Statement -> ForStatement
        abstract createForInStatement: initializer: ForInitializer * expression: Expression * statement: Statement -> ForInStatement
        abstract updateForInStatement: node: ForInStatement * initializer: ForInitializer * expression: Expression * statement: Statement -> ForInStatement
        abstract createForOfStatement: awaitModifier: AwaitKeyword option * initializer: ForInitializer * expression: Expression * statement: Statement -> ForOfStatement
        abstract updateForOfStatement: node: ForOfStatement * awaitModifier: AwaitKeyword option * initializer: ForInitializer * expression: Expression * statement: Statement -> ForOfStatement
        abstract createContinueStatement: ?label: U2<string, Identifier> -> ContinueStatement
        abstract updateContinueStatement: node: ContinueStatement * label: Identifier option -> ContinueStatement
        abstract createBreakStatement: ?label: U2<string, Identifier> -> BreakStatement
        abstract updateBreakStatement: node: BreakStatement * label: Identifier option -> BreakStatement
        abstract createReturnStatement: ?expression: Expression -> ReturnStatement
        abstract updateReturnStatement: node: ReturnStatement * expression: Expression option -> ReturnStatement
        abstract createWithStatement: expression: Expression * statement: Statement -> WithStatement
        abstract updateWithStatement: node: WithStatement * expression: Expression * statement: Statement -> WithStatement
        abstract createSwitchStatement: expression: Expression * caseBlock: CaseBlock -> SwitchStatement
        abstract updateSwitchStatement: node: SwitchStatement * expression: Expression * caseBlock: CaseBlock -> SwitchStatement
        abstract createLabeledStatement: label: U2<string, Identifier> * statement: Statement -> LabeledStatement
        abstract updateLabeledStatement: node: LabeledStatement * label: Identifier * statement: Statement -> LabeledStatement
        abstract createThrowStatement: expression: Expression -> ThrowStatement
        abstract updateThrowStatement: node: ThrowStatement * expression: Expression -> ThrowStatement
        abstract createTryStatement: tryBlock: Block * catchClause: CatchClause option * finallyBlock: Block option -> TryStatement
        abstract updateTryStatement: node: TryStatement * tryBlock: Block * catchClause: CatchClause option * finallyBlock: Block option -> TryStatement
        abstract createDebuggerStatement: unit -> DebuggerStatement
        abstract createVariableDeclaration: name: U2<string, BindingName> * ?exclamationToken: ExclamationToken * ?``type``: TypeNode * ?initializer: Expression -> VariableDeclaration
        abstract updateVariableDeclaration: node: VariableDeclaration * name: BindingName * exclamationToken: ExclamationToken option * ``type``: TypeNode option * initializer: Expression option -> VariableDeclaration
        abstract createVariableDeclarationList: declarations: VariableDeclaration[] * ?flags: NodeFlags -> VariableDeclarationList
        abstract updateVariableDeclarationList: node: VariableDeclarationList * declarations: VariableDeclaration[] -> VariableDeclarationList
        abstract createFunctionDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * asteriskToken: AsteriskToken option * name: U2<string, Identifier> option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option * body: Block option -> FunctionDeclaration
        abstract updateFunctionDeclaration: node: FunctionDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * asteriskToken: AsteriskToken option * name: Identifier option * typeParameters: TypeParameterDeclaration[] option * parameters: ParameterDeclaration[] * ``type``: TypeNode option * body: Block option -> FunctionDeclaration
        abstract createClassDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * name: U2<string, Identifier> option * typeParameters: TypeParameterDeclaration[] option * heritageClauses: HeritageClause[] option * members: ClassElement[] -> ClassDeclaration
        abstract updateClassDeclaration: node: ClassDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * name: Identifier option * typeParameters: TypeParameterDeclaration[] option * heritageClauses: HeritageClause[] option * members: ClassElement[] -> ClassDeclaration
        abstract createInterfaceDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * name: U2<string, Identifier> * typeParameters: TypeParameterDeclaration[] option * heritageClauses: HeritageClause[] option * members: TypeElement[] -> InterfaceDeclaration
        abstract updateInterfaceDeclaration: node: InterfaceDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * name: Identifier * typeParameters: TypeParameterDeclaration[] option * heritageClauses: HeritageClause[] option * members: TypeElement[] -> InterfaceDeclaration
        abstract createTypeAliasDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * name: U2<string, Identifier> * typeParameters: TypeParameterDeclaration[] option * ``type``: TypeNode -> TypeAliasDeclaration
        abstract updateTypeAliasDeclaration: node: TypeAliasDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * name: Identifier * typeParameters: TypeParameterDeclaration[] option * ``type``: TypeNode -> TypeAliasDeclaration
        abstract createEnumDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * name: U2<string, Identifier> * members: EnumMember[] -> EnumDeclaration
        abstract updateEnumDeclaration: node: EnumDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * name: Identifier * members: EnumMember[] -> EnumDeclaration
        abstract createModuleDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * name: ModuleName * body: ModuleBody option * ?flags: NodeFlags -> ModuleDeclaration
        abstract updateModuleDeclaration: node: ModuleDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * name: ModuleName * body: ModuleBody option -> ModuleDeclaration
        abstract createModuleBlock: statements: Statement[] -> ModuleBlock
        abstract updateModuleBlock: node: ModuleBlock * statements: Statement[] -> ModuleBlock
        abstract createCaseBlock: clauses: CaseOrDefaultClause[] -> CaseBlock
        abstract updateCaseBlock: node: CaseBlock * clauses: CaseOrDefaultClause[] -> CaseBlock
        abstract createNamespaceExportDeclaration: name: U2<string, Identifier> -> NamespaceExportDeclaration
        abstract updateNamespaceExportDeclaration: node: NamespaceExportDeclaration * name: Identifier -> NamespaceExportDeclaration
        abstract createImportEqualsDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * isTypeOnly: bool * name: U2<string, Identifier> * moduleReference: ModuleReference -> ImportEqualsDeclaration
        abstract updateImportEqualsDeclaration: node: ImportEqualsDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * isTypeOnly: bool * name: Identifier * moduleReference: ModuleReference -> ImportEqualsDeclaration
        abstract createImportDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * importClause: ImportClause option * moduleSpecifier: Expression * ?assertClause: AssertClause -> ImportDeclaration
        abstract updateImportDeclaration: node: ImportDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * importClause: ImportClause option * moduleSpecifier: Expression * assertClause: AssertClause option -> ImportDeclaration
        abstract createImportClause: isTypeOnly: bool * name: Identifier option * namedBindings: NamedImportBindings option -> ImportClause
        abstract updateImportClause: node: ImportClause * isTypeOnly: bool * name: Identifier option * namedBindings: NamedImportBindings option -> ImportClause
        abstract createAssertClause: elements: AssertEntry[] * ?multiLine: bool -> AssertClause
        abstract updateAssertClause: node: AssertClause * elements: AssertEntry[] * ?multiLine: bool -> AssertClause
        abstract createAssertEntry: name: AssertionKey * value: Expression -> AssertEntry
        abstract updateAssertEntry: node: AssertEntry * name: AssertionKey * value: Expression -> AssertEntry
        abstract createNamespaceImport: name: Identifier -> NamespaceImport
        abstract updateNamespaceImport: node: NamespaceImport * name: Identifier -> NamespaceImport
        abstract createNamespaceExport: name: Identifier -> NamespaceExport
        abstract updateNamespaceExport: node: NamespaceExport * name: Identifier -> NamespaceExport
        abstract createNamedImports: elements: ImportSpecifier[] -> NamedImports
        abstract updateNamedImports: node: NamedImports * elements: ImportSpecifier[] -> NamedImports
        abstract createImportSpecifier: isTypeOnly: bool * propertyName: Identifier option * name: Identifier -> ImportSpecifier
        abstract updateImportSpecifier: node: ImportSpecifier * isTypeOnly: bool * propertyName: Identifier option * name: Identifier -> ImportSpecifier
        abstract createExportAssignment: decorators: Decorator[] option * modifiers: Modifier[] option * isExportEquals: bool option * expression: Expression -> ExportAssignment
        abstract updateExportAssignment: node: ExportAssignment * decorators: Decorator[] option * modifiers: Modifier[] option * expression: Expression -> ExportAssignment
        abstract createExportDeclaration: decorators: Decorator[] option * modifiers: Modifier[] option * isTypeOnly: bool * exportClause: NamedExportBindings option * ?moduleSpecifier: Expression * ?assertClause: AssertClause -> ExportDeclaration
        abstract updateExportDeclaration: node: ExportDeclaration * decorators: Decorator[] option * modifiers: Modifier[] option * isTypeOnly: bool * exportClause: NamedExportBindings option * moduleSpecifier: Expression option * assertClause: AssertClause option -> ExportDeclaration
        abstract createNamedExports: elements: ExportSpecifier[] -> NamedExports
        abstract updateNamedExports: node: NamedExports * elements: ExportSpecifier[] -> NamedExports
        abstract createExportSpecifier: isTypeOnly: bool * propertyName: U2<string, Identifier> option * name: U2<string, Identifier> -> ExportSpecifier
        abstract updateExportSpecifier: node: ExportSpecifier * isTypeOnly: bool * propertyName: Identifier option * name: Identifier -> ExportSpecifier
        abstract createExternalModuleReference: expression: Expression -> ExternalModuleReference
        abstract updateExternalModuleReference: node: ExternalModuleReference * expression: Expression -> ExternalModuleReference
        abstract createJSDocAllType: unit -> JSDocAllType
        abstract createJSDocUnknownType: unit -> JSDocUnknownType
        abstract createJSDocNonNullableType: ``type``: TypeNode -> JSDocNonNullableType
        abstract updateJSDocNonNullableType: node: JSDocNonNullableType * ``type``: TypeNode -> JSDocNonNullableType
        abstract createJSDocNullableType: ``type``: TypeNode -> JSDocNullableType
        abstract updateJSDocNullableType: node: JSDocNullableType * ``type``: TypeNode -> JSDocNullableType
        abstract createJSDocOptionalType: ``type``: TypeNode -> JSDocOptionalType
        abstract updateJSDocOptionalType: node: JSDocOptionalType * ``type``: TypeNode -> JSDocOptionalType
        abstract createJSDocFunctionType: parameters: ParameterDeclaration[] * ``type``: TypeNode option -> JSDocFunctionType
        abstract updateJSDocFunctionType: node: JSDocFunctionType * parameters: ParameterDeclaration[] * ``type``: TypeNode option -> JSDocFunctionType
        abstract createJSDocVariadicType: ``type``: TypeNode -> JSDocVariadicType
        abstract updateJSDocVariadicType: node: JSDocVariadicType * ``type``: TypeNode -> JSDocVariadicType
        abstract createJSDocNamepathType: ``type``: TypeNode -> JSDocNamepathType
        abstract updateJSDocNamepathType: node: JSDocNamepathType * ``type``: TypeNode -> JSDocNamepathType
        abstract createJSDocTypeExpression: ``type``: TypeNode -> JSDocTypeExpression
        abstract updateJSDocTypeExpression: node: JSDocTypeExpression * ``type``: TypeNode -> JSDocTypeExpression
        abstract createJSDocNameReference: name: U2<EntityName, JSDocMemberName> -> JSDocNameReference
        abstract updateJSDocNameReference: node: JSDocNameReference * name: U2<EntityName, JSDocMemberName> -> JSDocNameReference
        abstract createJSDocMemberName: left: U2<EntityName, JSDocMemberName> * right: Identifier -> JSDocMemberName
        abstract updateJSDocMemberName: node: JSDocMemberName * left: U2<EntityName, JSDocMemberName> * right: Identifier -> JSDocMemberName
        abstract createJSDocLink: name: U2<EntityName, JSDocMemberName> option * text: string -> JSDocLink
        abstract updateJSDocLink: node: JSDocLink * name: U2<EntityName, JSDocMemberName> option * text: string -> JSDocLink
        abstract createJSDocLinkCode: name: U2<EntityName, JSDocMemberName> option * text: string -> JSDocLinkCode
        abstract updateJSDocLinkCode: node: JSDocLinkCode * name: U2<EntityName, JSDocMemberName> option * text: string -> JSDocLinkCode
        abstract createJSDocLinkPlain: name: U2<EntityName, JSDocMemberName> option * text: string -> JSDocLinkPlain
        abstract updateJSDocLinkPlain: node: JSDocLinkPlain * name: U2<EntityName, JSDocMemberName> option * text: string -> JSDocLinkPlain
        abstract createJSDocTypeLiteral: ?jsDocPropertyTags: JSDocPropertyLikeTag[] * ?isArrayType: bool -> JSDocTypeLiteral
        abstract updateJSDocTypeLiteral: node: JSDocTypeLiteral * jsDocPropertyTags: JSDocPropertyLikeTag[] option * isArrayType: bool option -> JSDocTypeLiteral
        abstract createJSDocSignature: typeParameters: JSDocTemplateTag[] option * parameters: JSDocParameterTag[] * ?``type``: JSDocReturnTag -> JSDocSignature
        abstract updateJSDocSignature: node: JSDocSignature * typeParameters: JSDocTemplateTag[] option * parameters: JSDocParameterTag[] * ``type``: JSDocReturnTag option -> JSDocSignature
        abstract createJSDocTemplateTag: tagName: Identifier option * ``constraint``: JSDocTypeExpression option * typeParameters: TypeParameterDeclaration[] * ?comment: U2<string, JSDocComment[]> -> JSDocTemplateTag
        abstract updateJSDocTemplateTag: node: JSDocTemplateTag * tagName: Identifier option * ``constraint``: JSDocTypeExpression option * typeParameters: TypeParameterDeclaration[] * comment: U2<string, JSDocComment[]> option -> JSDocTemplateTag
        abstract createJSDocTypedefTag: tagName: Identifier option * ?typeExpression: U2<JSDocTypeExpression, JSDocTypeLiteral> * ?fullName: U2<Identifier, JSDocNamespaceDeclaration> * ?comment: U2<string, JSDocComment[]> -> JSDocTypedefTag
        abstract updateJSDocTypedefTag: node: JSDocTypedefTag * tagName: Identifier option * typeExpression: U2<JSDocTypeExpression, JSDocTypeLiteral> option * fullName: U2<Identifier, JSDocNamespaceDeclaration> option * comment: U2<string, JSDocComment[]> option -> JSDocTypedefTag
        abstract createJSDocParameterTag: tagName: Identifier option * name: EntityName * isBracketed: bool * ?typeExpression: JSDocTypeExpression * ?isNameFirst: bool * ?comment: U2<string, JSDocComment[]> -> JSDocParameterTag
        abstract updateJSDocParameterTag: node: JSDocParameterTag * tagName: Identifier option * name: EntityName * isBracketed: bool * typeExpression: JSDocTypeExpression option * isNameFirst: bool * comment: U2<string, JSDocComment[]> option -> JSDocParameterTag
        abstract createJSDocPropertyTag: tagName: Identifier option * name: EntityName * isBracketed: bool * ?typeExpression: JSDocTypeExpression * ?isNameFirst: bool * ?comment: U2<string, JSDocComment[]> -> JSDocPropertyTag
        abstract updateJSDocPropertyTag: node: JSDocPropertyTag * tagName: Identifier option * name: EntityName * isBracketed: bool * typeExpression: JSDocTypeExpression option * isNameFirst: bool * comment: U2<string, JSDocComment[]> option -> JSDocPropertyTag
        abstract createJSDocTypeTag: tagName: Identifier option * typeExpression: JSDocTypeExpression * ?comment: U2<string, JSDocComment[]> -> JSDocTypeTag
        abstract updateJSDocTypeTag: node: JSDocTypeTag * tagName: Identifier option * typeExpression: JSDocTypeExpression * comment: U2<string, JSDocComment[]> option -> JSDocTypeTag
        abstract createJSDocSeeTag: tagName: Identifier option * nameExpression: JSDocNameReference option * ?comment: U2<string, JSDocComment[]> -> JSDocSeeTag
        abstract updateJSDocSeeTag: node: JSDocSeeTag * tagName: Identifier option * nameExpression: JSDocNameReference option * ?comment: U2<string, JSDocComment[]> -> JSDocSeeTag
        abstract createJSDocReturnTag: tagName: Identifier option * ?typeExpression: JSDocTypeExpression * ?comment: U2<string, JSDocComment[]> -> JSDocReturnTag
        abstract updateJSDocReturnTag: node: JSDocReturnTag * tagName: Identifier option * typeExpression: JSDocTypeExpression option * comment: U2<string, JSDocComment[]> option -> JSDocReturnTag
        abstract createJSDocThisTag: tagName: Identifier option * typeExpression: JSDocTypeExpression * ?comment: U2<string, JSDocComment[]> -> JSDocThisTag
        abstract updateJSDocThisTag: node: JSDocThisTag * tagName: Identifier option * typeExpression: JSDocTypeExpression option * comment: U2<string, JSDocComment[]> option -> JSDocThisTag
        abstract createJSDocEnumTag: tagName: Identifier option * typeExpression: JSDocTypeExpression * ?comment: U2<string, JSDocComment[]> -> JSDocEnumTag
        abstract updateJSDocEnumTag: node: JSDocEnumTag * tagName: Identifier option * typeExpression: JSDocTypeExpression * comment: U2<string, JSDocComment[]> option -> JSDocEnumTag
        abstract createJSDocCallbackTag: tagName: Identifier option * typeExpression: JSDocSignature * ?fullName: U2<Identifier, JSDocNamespaceDeclaration> * ?comment: U2<string, JSDocComment[]> -> JSDocCallbackTag
        abstract updateJSDocCallbackTag: node: JSDocCallbackTag * tagName: Identifier option * typeExpression: JSDocSignature * fullName: U2<Identifier, JSDocNamespaceDeclaration> option * comment: U2<string, JSDocComment[]> option -> JSDocCallbackTag
        abstract createJSDocAugmentsTag: tagName: Identifier option * className: obj * ?comment: U2<string, JSDocComment[]> -> JSDocAugmentsTag
        abstract updateJSDocAugmentsTag: node: JSDocAugmentsTag * tagName: Identifier option * className: obj * comment: U2<string, JSDocComment[]> option -> JSDocAugmentsTag
        abstract createJSDocImplementsTag: tagName: Identifier option * className: obj * ?comment: U2<string, JSDocComment[]> -> JSDocImplementsTag
        abstract updateJSDocImplementsTag: node: JSDocImplementsTag * tagName: Identifier option * className: obj * comment: U2<string, JSDocComment[]> option -> JSDocImplementsTag
        abstract createJSDocAuthorTag: tagName: Identifier option * ?comment: U2<string, JSDocComment[]> -> JSDocAuthorTag
        abstract updateJSDocAuthorTag: node: JSDocAuthorTag * tagName: Identifier option * comment: U2<string, JSDocComment[]> option -> JSDocAuthorTag
        abstract createJSDocClassTag: tagName: Identifier option * ?comment: U2<string, JSDocComment[]> -> JSDocClassTag
        abstract updateJSDocClassTag: node: JSDocClassTag * tagName: Identifier option * comment: U2<string, JSDocComment[]> option -> JSDocClassTag
        abstract createJSDocPublicTag: tagName: Identifier option * ?comment: U2<string, JSDocComment[]> -> JSDocPublicTag
        abstract updateJSDocPublicTag: node: JSDocPublicTag * tagName: Identifier option * comment: U2<string, JSDocComment[]> option -> JSDocPublicTag
        abstract createJSDocPrivateTag: tagName: Identifier option * ?comment: U2<string, JSDocComment[]> -> JSDocPrivateTag
        abstract updateJSDocPrivateTag: node: JSDocPrivateTag * tagName: Identifier option * comment: U2<string, JSDocComment[]> option -> JSDocPrivateTag
        abstract createJSDocProtectedTag: tagName: Identifier option * ?comment: U2<string, JSDocComment[]> -> JSDocProtectedTag
        abstract updateJSDocProtectedTag: node: JSDocProtectedTag * tagName: Identifier option * comment: U2<string, JSDocComment[]> option -> JSDocProtectedTag
        abstract createJSDocReadonlyTag: tagName: Identifier option * ?comment: U2<string, JSDocComment[]> -> JSDocReadonlyTag
        abstract updateJSDocReadonlyTag: node: JSDocReadonlyTag * tagName: Identifier option * comment: U2<string, JSDocComment[]> option -> JSDocReadonlyTag
        abstract createJSDocUnknownTag: tagName: Identifier * ?comment: U2<string, JSDocComment[]> -> JSDocUnknownTag
        abstract updateJSDocUnknownTag: node: JSDocUnknownTag * tagName: Identifier * comment: U2<string, JSDocComment[]> option -> JSDocUnknownTag
        abstract createJSDocDeprecatedTag: tagName: Identifier * ?comment: U2<string, JSDocComment[]> -> JSDocDeprecatedTag
        abstract updateJSDocDeprecatedTag: node: JSDocDeprecatedTag * tagName: Identifier * ?comment: U2<string, JSDocComment[]> -> JSDocDeprecatedTag
        abstract createJSDocOverrideTag: tagName: Identifier * ?comment: U2<string, JSDocComment[]> -> JSDocOverrideTag
        abstract updateJSDocOverrideTag: node: JSDocOverrideTag * tagName: Identifier * ?comment: U2<string, JSDocComment[]> -> JSDocOverrideTag
        abstract createJSDocText: text: string -> JSDocText
        abstract updateJSDocText: node: JSDocText * text: string -> JSDocText
        abstract createJSDocComment: ?comment: U2<string, JSDocComment[]> * ?tags: JSDocTag[] -> JSDoc
        abstract updateJSDocComment: node: JSDoc * comment: U2<string, JSDocComment[]> option * tags: JSDocTag[] option -> JSDoc
        abstract createJsxElement: openingElement: JsxOpeningElement * children: JsxChild[] * closingElement: JsxClosingElement -> JsxElement
        abstract updateJsxElement: node: JsxElement * openingElement: JsxOpeningElement * children: JsxChild[] * closingElement: JsxClosingElement -> JsxElement
        abstract createJsxSelfClosingElement: tagName: JsxTagNameExpression * typeArguments: TypeNode[] option * attributes: JsxAttributes -> JsxSelfClosingElement
        abstract updateJsxSelfClosingElement: node: JsxSelfClosingElement * tagName: JsxTagNameExpression * typeArguments: TypeNode[] option * attributes: JsxAttributes -> JsxSelfClosingElement
        abstract createJsxOpeningElement: tagName: JsxTagNameExpression * typeArguments: TypeNode[] option * attributes: JsxAttributes -> JsxOpeningElement
        abstract updateJsxOpeningElement: node: JsxOpeningElement * tagName: JsxTagNameExpression * typeArguments: TypeNode[] option * attributes: JsxAttributes -> JsxOpeningElement
        abstract createJsxClosingElement: tagName: JsxTagNameExpression -> JsxClosingElement
        abstract updateJsxClosingElement: node: JsxClosingElement * tagName: JsxTagNameExpression -> JsxClosingElement
        abstract createJsxFragment: openingFragment: JsxOpeningFragment * children: JsxChild[] * closingFragment: JsxClosingFragment -> JsxFragment
        abstract createJsxText: text: string * ?containsOnlyTriviaWhiteSpaces: bool -> JsxText
        abstract updateJsxText: node: JsxText * text: string * ?containsOnlyTriviaWhiteSpaces: bool -> JsxText
        abstract createJsxOpeningFragment: unit -> JsxOpeningFragment
        abstract createJsxJsxClosingFragment: unit -> JsxClosingFragment
        abstract updateJsxFragment: node: JsxFragment * openingFragment: JsxOpeningFragment * children: JsxChild[] * closingFragment: JsxClosingFragment -> JsxFragment
        abstract createJsxAttribute: name: Identifier * initializer: U2<StringLiteral, JsxExpression> option -> JsxAttribute
        abstract updateJsxAttribute: node: JsxAttribute * name: Identifier * initializer: U2<StringLiteral, JsxExpression> option -> JsxAttribute
        abstract createJsxAttributes: properties: JsxAttributeLike[] -> JsxAttributes
        abstract updateJsxAttributes: node: JsxAttributes * properties: JsxAttributeLike[] -> JsxAttributes
        abstract createJsxSpreadAttribute: expression: Expression -> JsxSpreadAttribute
        abstract updateJsxSpreadAttribute: node: JsxSpreadAttribute * expression: Expression -> JsxSpreadAttribute
        abstract createJsxExpression: dotDotDotToken: DotDotDotToken option * expression: Expression option -> JsxExpression
        abstract updateJsxExpression: node: JsxExpression * expression: Expression option -> JsxExpression
        abstract createCaseClause: expression: Expression * statements: Statement[] -> CaseClause
        abstract updateCaseClause: node: CaseClause * expression: Expression * statements: Statement[] -> CaseClause
        abstract createDefaultClause: statements: Statement[] -> DefaultClause
        abstract updateDefaultClause: node: DefaultClause * statements: Statement[] -> DefaultClause
        abstract createHeritageClause: token: obj * types: ExpressionWithTypeArguments[] -> HeritageClause
        abstract updateHeritageClause: node: HeritageClause * types: ExpressionWithTypeArguments[] -> HeritageClause
        abstract createCatchClause: variableDeclaration: U3<string, BindingName, VariableDeclaration> option * block: Block -> CatchClause
        abstract updateCatchClause: node: CatchClause * variableDeclaration: VariableDeclaration option * block: Block -> CatchClause
        abstract createPropertyAssignment: name: U2<string, PropertyName> * initializer: Expression -> PropertyAssignment
        abstract updatePropertyAssignment: node: PropertyAssignment * name: PropertyName * initializer: Expression -> PropertyAssignment
        abstract createShorthandPropertyAssignment: name: U2<string, Identifier> * ?objectAssignmentInitializer: Expression -> ShorthandPropertyAssignment
        abstract updateShorthandPropertyAssignment: node: ShorthandPropertyAssignment * name: Identifier * objectAssignmentInitializer: Expression option -> ShorthandPropertyAssignment
        abstract createSpreadAssignment: expression: Expression -> SpreadAssignment
        abstract updateSpreadAssignment: node: SpreadAssignment * expression: Expression -> SpreadAssignment
        abstract createEnumMember: name: U2<string, PropertyName> * ?initializer: Expression -> EnumMember
        abstract updateEnumMember: node: EnumMember * name: PropertyName * initializer: Expression option -> EnumMember
        abstract createSourceFile: statements: Statement[] * endOfFileToken: EndOfFileToken * flags: NodeFlags -> SourceFile
        abstract updateSourceFile: node: SourceFile * statements: Statement[] * ?isDeclarationFile: bool * ?referencedFiles: FileReference[] * ?typeReferences: FileReference[] * ?hasNoDefaultLib: bool * ?libReferences: FileReference[] -> SourceFile
        abstract createNotEmittedStatement: original: Node -> NotEmittedStatement
        abstract createPartiallyEmittedExpression: expression: Expression * ?original: Node -> PartiallyEmittedExpression
        abstract updatePartiallyEmittedExpression: node: PartiallyEmittedExpression * expression: Expression -> PartiallyEmittedExpression
        abstract createCommaListExpression: elements: Expression[] -> CommaListExpression
        abstract updateCommaListExpression: node: CommaListExpression * elements: Expression[] -> CommaListExpression
        abstract createBundle: sourceFiles: SourceFile[] * ?prepends: U2<UnparsedSource, InputFiles>[] -> Bundle
        abstract updateBundle: node: Bundle * sourceFiles: SourceFile[] * ?prepends: U2<UnparsedSource, InputFiles>[] -> Bundle
        abstract createComma: left: Expression * right: Expression -> BinaryExpression
        abstract createAssignment: left: U2<ObjectLiteralExpression, ArrayLiteralExpression> * right: Expression -> DestructuringAssignment
        abstract createAssignment: left: Expression * right: Expression -> AssignmentExpression<EqualsToken>
        abstract createLogicalOr: left: Expression * right: Expression -> BinaryExpression
        abstract createLogicalAnd: left: Expression * right: Expression -> BinaryExpression
        abstract createBitwiseOr: left: Expression * right: Expression -> BinaryExpression
        abstract createBitwiseXor: left: Expression * right: Expression -> BinaryExpression
        abstract createBitwiseAnd: left: Expression * right: Expression -> BinaryExpression
        abstract createStrictEquality: left: Expression * right: Expression -> BinaryExpression
        abstract createStrictInequality: left: Expression * right: Expression -> BinaryExpression
        abstract createEquality: left: Expression * right: Expression -> BinaryExpression
        abstract createInequality: left: Expression * right: Expression -> BinaryExpression
        abstract createLessThan: left: Expression * right: Expression -> BinaryExpression
        abstract createLessThanEquals: left: Expression * right: Expression -> BinaryExpression
        abstract createGreaterThan: left: Expression * right: Expression -> BinaryExpression
        abstract createGreaterThanEquals: left: Expression * right: Expression -> BinaryExpression
        abstract createLeftShift: left: Expression * right: Expression -> BinaryExpression
        abstract createRightShift: left: Expression * right: Expression -> BinaryExpression
        abstract createUnsignedRightShift: left: Expression * right: Expression -> BinaryExpression
        abstract createAdd: left: Expression * right: Expression -> BinaryExpression
        abstract createSubtract: left: Expression * right: Expression -> BinaryExpression
        abstract createMultiply: left: Expression * right: Expression -> BinaryExpression
        abstract createDivide: left: Expression * right: Expression -> BinaryExpression
        abstract createModulo: left: Expression * right: Expression -> BinaryExpression
        abstract createExponent: left: Expression * right: Expression -> BinaryExpression
        abstract createPrefixPlus: operand: Expression -> PrefixUnaryExpression
        abstract createPrefixMinus: operand: Expression -> PrefixUnaryExpression
        abstract createPrefixIncrement: operand: Expression -> PrefixUnaryExpression
        abstract createPrefixDecrement: operand: Expression -> PrefixUnaryExpression
        abstract createBitwiseNot: operand: Expression -> PrefixUnaryExpression
        abstract createLogicalNot: operand: Expression -> PrefixUnaryExpression
        abstract createPostfixIncrement: operand: Expression -> PostfixUnaryExpression
        abstract createPostfixDecrement: operand: Expression -> PostfixUnaryExpression
        abstract createImmediatelyInvokedFunctionExpression: statements: Statement[] -> CallExpression
        abstract createImmediatelyInvokedFunctionExpression: statements: Statement[] * param: ParameterDeclaration * paramValue: Expression -> CallExpression
        abstract createImmediatelyInvokedArrowFunction: statements: Statement[] -> CallExpression
        abstract createImmediatelyInvokedArrowFunction: statements: Statement[] * param: ParameterDeclaration * paramValue: Expression -> CallExpression
        abstract createVoidZero: unit -> VoidExpression
        abstract createExportDefault: expression: Expression -> ExportAssignment
        abstract createExternalModuleExport: exportName: Identifier -> ExportDeclaration
        abstract restoreOuterExpressions: outerExpression: Expression option * innerExpression: Expression * ?kinds: OuterExpressionKinds -> Expression

    type [<AllowNullLiteral>] CoreTransformationContext =
        abstract factory: NodeFactory
        /// Gets the compiler options supplied to the transformer.
        abstract getCompilerOptions: unit -> CompilerOptions
        /// Starts a new lexical environment.
        abstract startLexicalEnvironment: unit -> unit
        /// Suspends the current lexical environment, usually after visiting a parameter list.
        abstract suspendLexicalEnvironment: unit -> unit
        /// Resumes a suspended lexical environment, usually before visiting a function body.
        abstract resumeLexicalEnvironment: unit -> unit
        /// Ends a lexical environment, returning any declarations.
        abstract endLexicalEnvironment: unit -> Statement[] option
        /// Hoists a function declaration to the containing scope.
        abstract hoistFunctionDeclaration: node: FunctionDeclaration -> unit
        /// Hoists a variable declaration to the containing scope.
        abstract hoistVariableDeclaration: node: Identifier -> unit

    type [<AllowNullLiteral>] TransformationContext =
        inherit CoreTransformationContext
        /// Records a request for a non-scoped emit helper in the current context.
        abstract requestEmitHelper: helper: EmitHelper -> unit
        /// Gets and resets the requested non-scoped emit helpers.
        abstract readEmitHelpers: unit -> EmitHelper[] option
        /// Enables expression substitutions in the pretty printer for the provided SyntaxKind.
        abstract enableSubstitution: kind: SyntaxKind -> unit
        /// Determines whether expression substitutions are enabled for the provided node.
        abstract isSubstitutionEnabled: node: Node -> bool
        /// <summary>
        /// Hook used by transformers to substitute expressions just before they
        /// are emitted by the pretty printer.
        ///
        /// NOTE: Transformation hooks should only be modified during <c>Transformer</c> initialization,
        /// before returning the <c>NodeTransformer</c> callback.
        /// </summary>
        abstract onSubstituteNode: (EmitHint -> Node -> Node) with get, set
        /// Enables before/after emit notifications in the pretty printer for the provided
        /// SyntaxKind.
        abstract enableEmitNotification: kind: SyntaxKind -> unit
        /// Determines whether before/after emit notifications should be raised in the pretty
        /// printer when it emits a node.
        abstract isEmitNotificationEnabled: node: Node -> bool
        /// <summary>
        /// Hook used to allow transformers to capture state before or after
        /// the printer emits a node.
        ///
        /// NOTE: Transformation hooks should only be modified during <c>Transformer</c> initialization,
        /// before returning the <c>NodeTransformer</c> callback.
        /// </summary>
        abstract onEmitNode: (EmitHint -> Node -> (EmitHint -> Node -> unit) -> unit) with get, set

    type [<AllowNullLiteral>] TransformationResult<'T> =
        /// Gets the transformed source files.
        abstract transformed: 'T[] with get, set
        /// Gets diagnostics for the transformation.
        abstract diagnostics: DiagnosticWithLocation[] option with get, set
        /// <summary>Gets a substitute for a node, if one is available; otherwise, returns the original node.</summary>
        /// <param name="hint">A hint as to the intended usage of the node.</param>
        /// <param name="node">The node to substitute.</param>
        abstract substituteNode: hint: EmitHint * node: Node -> Node
        /// <summary>Emits a node with possible notification.</summary>
        /// <param name="hint">A hint as to the intended usage of the node.</param>
        /// <param name="node">The node to emit.</param>
        /// <param name="emitCallback">A callback used to emit the node.</param>
        abstract emitNodeWithNotification: hint: EmitHint * node: Node * emitCallback: (EmitHint -> Node -> unit) -> unit
        /// <summary>Indicates if a given node needs an emit notification</summary>
        /// <param name="node">The node to emit.</param>
        abstract isEmitNotificationEnabled: node: Node -> bool
        /// Clean up EmitNode entries on any parse-tree nodes.
        abstract dispose: unit -> unit

    /// <summary>
    /// A function that is used to initialize and return a <c>Transformer</c> callback, which in turn
    /// will be used to transform one or more nodes.
    /// </summary>
    type [<AllowNullLiteral>] TransformerFactory<'T> =
        /// <summary>
        /// A function that is used to initialize and return a <c>Transformer</c> callback, which in turn
        /// will be used to transform one or more nodes.
        /// </summary>
        [<Emit "$0($1...)">] abstract Invoke: context: TransformationContext -> Transformer<'T>

    /// A function that transforms a node.
    type [<AllowNullLiteral>] Transformer<'T> =
        /// A function that transforms a node.
        [<Emit "$0($1...)">] abstract Invoke: node: 'T -> 'T

    /// A function that accepts and possibly transforms a node.
    type [<AllowNullLiteral>] Visitor =
        /// A function that accepts and possibly transforms a node.
        [<Emit "$0($1...)">] abstract Invoke: node: Node -> VisitResult<Node>

    type [<AllowNullLiteral>] NodeVisitor =
        [<Emit "$0($1...)">] abstract Invoke: nodes: 'T * visitor: Visitor option * ?test: (Node -> bool) * ?lift: (Node[] -> 'T) -> 'T
        [<Emit "$0($1...)">] abstract Invoke: nodes: 'T option * visitor: Visitor option * ?test: (Node -> bool) * ?lift: (Node[] -> 'T) -> 'T option

    type [<AllowNullLiteral>] NodesVisitor =
        [<Emit "$0($1...)">] abstract Invoke: nodes: 'T[] * visitor: Visitor option * ?test: (Node -> bool) * ?start: float * ?count: float -> 'T[]
        [<Emit "$0($1...)">] abstract Invoke: nodes: 'T[] option * visitor: Visitor option * ?test: (Node -> bool) * ?start: float * ?count: float -> 'T[] option

    type VisitResult<'T> =
        U2<'T, 'T[]> option

    type [<AllowNullLiteral>] Printer =
        /// <summary>Print a node and its subtree as-is, without any emit transformations.</summary>
        /// <param name="hint">
        /// A value indicating the purpose of a node. This is primarily used to
        /// distinguish between an <c>Identifier</c> used in an expression position, versus an
        /// <c>Identifier</c> used as an <c>IdentifierName</c> as part of a declaration. For most nodes you
        /// should just pass <c>Unspecified</c>.
        /// </param>
        /// <param name="node">
        /// The node to print. The node and its subtree are printed as-is, without any
        /// emit transformations.
        /// </param>
        /// <param name="sourceFile">
        /// A source file that provides context for the node. The source text of
        /// the file is used to emit the original source content for literals and identifiers, while
        /// the identifiers of the source file are used when generating unique names to avoid
        /// collisions.
        /// </param>
        abstract printNode: hint: EmitHint * node: Node * sourceFile: SourceFile -> string
        /// Prints a list of nodes using the given format flags
        abstract printList: format: ListFormat * list: 'T[] * sourceFile: SourceFile -> string
        /// Prints a source file as-is, without any emit transformations.
        abstract printFile: sourceFile: SourceFile -> string
        /// Prints a bundle of source files as-is, without any emit transformations.
        abstract printBundle: bundle: Bundle -> string

    type [<AllowNullLiteral>] PrintHandlers =
        /// A hook used by the Printer when generating unique names to avoid collisions with
        /// globally defined names that exist outside of the current source file.
        abstract hasGlobalName: name: string -> bool
        /// <summary>
        /// A hook used by the Printer to provide notifications prior to emitting a node. A
        /// compatible implementation **must** invoke <c>emitCallback</c> with the provided <c>hint</c> and
        /// <c>node</c> values.
        /// </summary>
        /// <param name="hint">A hint indicating the intended purpose of the node.</param>
        /// <param name="node">The node to emit.</param>
        /// <param name="emitCallback">A callback that, when invoked, will emit the node.</param>
        /// <example>
        /// <code lang="ts">
        /// var printer = createPrinter(printerOptions, {
        ///   onEmitNode(hint, node, emitCallback) {
        ///     // set up or track state prior to emitting the node...
        ///     emitCallback(hint, node);
        ///     // restore state after emitting the node...
        ///   }
        /// });
        /// </code>
        /// </example>
        abstract onEmitNode: hint: EmitHint * node: Node * emitCallback: (EmitHint -> Node -> unit) -> unit
        /// <summary>A hook used to check if an emit notification is required for a node.</summary>
        /// <param name="node">The node to emit.</param>
        abstract isEmitNotificationEnabled: node: Node -> bool
        /// <summary>
        /// A hook used by the Printer to perform just-in-time substitution of a node. This is
        /// primarily used by node transformations that need to substitute one node for another,
        /// such as replacing <c>myExportedVar</c> with <c>exports.myExportedVar</c>.
        /// </summary>
        /// <param name="hint">A hint indicating the intended purpose of the node.</param>
        /// <param name="node">The node to emit.</param>
        /// <example>
        /// <code lang="ts">
        /// var printer = createPrinter(printerOptions, {
        ///   substituteNode(hint, node) {
        ///     // perform substitution if necessary...
        ///     return node;
        ///   }
        /// });
        /// </code>
        /// </example>
        abstract substituteNode: hint: EmitHint * node: Node -> Node

    type [<AllowNullLiteral>] PrinterOptions =
        abstract removeComments: bool option with get, set
        abstract newLine: NewLineKind option with get, set
        abstract omitTrailingSemicolon: bool option with get, set
        abstract noEmitHelpers: bool option with get, set

    type [<AllowNullLiteral>] GetEffectiveTypeRootsHost =
        abstract directoryExists: directoryName: string -> bool
        abstract getCurrentDirectory: unit -> string

    type [<AllowNullLiteral>] TextSpan =
        abstract start: float with get, set
        abstract length: float with get, set

    type [<AllowNullLiteral>] TextChangeRange =
        abstract span: TextSpan with get, set
        abstract newLength: float with get, set

    type [<AllowNullLiteral>] SyntaxList =
        inherit Node
        abstract kind: SyntaxKind with get, set
        abstract _children: Node[] with get, set

    type [<RequireQualifiedAccess>] ListFormat =
        | None = 0
        | SingleLine = 0
        | MultiLine = 1
        | PreserveLines = 2
        | LinesMask = 3
        | NotDelimited = 0
        | BarDelimited = 4
        | AmpersandDelimited = 8
        | CommaDelimited = 16
        | AsteriskDelimited = 32
        | DelimitersMask = 60
        | AllowTrailingComma = 64
        | Indented = 128
        | SpaceBetweenBraces = 256
        | SpaceBetweenSiblings = 512
        | Braces = 1024
        | Parenthesis = 2048
        | AngleBrackets = 4096
        | SquareBrackets = 8192
        | BracketsMask = 15360
        | OptionalIfUndefined = 16384
        | OptionalIfEmpty = 32768
        | Optional = 49152
        | PreferNewLine = 65536
        | NoTrailingNewLine = 131072
        | NoInterveningComments = 262144
        | NoSpaceIfEmpty = 524288
        | SingleElement = 1048576
        | SpaceAfterList = 2097152
        | Modifiers = 262656
        | HeritageClauses = 512
        | SingleLineTypeLiteralMembers = 768
        | MultiLineTypeLiteralMembers = 32897
        | SingleLineTupleTypeElements = 528
        | MultiLineTupleTypeElements = 657
        | UnionTypeConstituents = 516
        | IntersectionTypeConstituents = 520
        | ObjectBindingPatternElements = 525136
        | ArrayBindingPatternElements = 524880
        | ObjectLiteralExpressionProperties = 526226
        | ImportClauseEntries = 526226
        | ArrayLiteralExpressionElements = 8914
        | CommaListElements = 528
        | CallExpressionArguments = 2576
        | NewExpressionArguments = 18960
        | TemplateExpressionSpans = 262144
        | SingleLineBlockStatements = 768
        | MultiLineBlockStatements = 129
        | VariableDeclarationList = 528
        | SingleLineFunctionBodyStatements = 768
        | MultiLineFunctionBodyStatements = 1
        | ClassHeritageClauses = 0
        | ClassMembers = 129
        | InterfaceMembers = 129
        | EnumMembers = 145
        | CaseBlockClauses = 129
        | NamedImportsOrExportsElements = 525136
        | JsxElementOrFragmentChildren = 262144
        | JsxElementAttributes = 262656
        | CaseOrDefaultClauseStatements = 163969
        | HeritageClauseTypes = 528
        | SourceFileStatements = 131073
        | Decorators = 2146305
        | TypeArguments = 53776
        | TypeParameters = 53776
        | Parameters = 2576
        | IndexSignatureParameters = 8848
        | JSDocComment = 33

    type [<AllowNullLiteral>] UserPreferences =
        abstract disableSuggestions: bool option
        abstract quotePreference: UserPreferencesQuotePreference option
        abstract includeCompletionsForModuleExports: bool option
        abstract includeCompletionsForImportStatements: bool option
        abstract includeCompletionsWithSnippetText: bool option
        abstract includeAutomaticOptionalChainCompletions: bool option
        abstract includeCompletionsWithInsertText: bool option
        abstract includeCompletionsWithClassMemberSnippets: bool option
        abstract allowIncompleteCompletions: bool option
        abstract importModuleSpecifierPreference: UserPreferencesImportModuleSpecifierPreference option
        /// <summary>Determines whether we import <c>foo/index.ts</c> as "foo", "foo/index", or "foo/index.js"</summary>
        abstract importModuleSpecifierEnding: UserPreferencesImportModuleSpecifierEnding option
        abstract allowTextChangesInNewFiles: bool option
        abstract providePrefixAndSuffixTextForRename: bool option
        abstract includePackageJsonAutoImports: UserPreferencesIncludePackageJsonAutoImports option
        abstract provideRefactorNotApplicableReason: bool option
        abstract jsxAttributeCompletionStyle: UserPreferencesJsxAttributeCompletionStyle option

    /// Represents a bigint literal value without requiring bigint support
    type [<AllowNullLiteral>] PseudoBigInt =
        abstract negative: bool with get, set
        abstract base10Value: string with get, set

    type [<RequireQualifiedAccess>] FileWatcherEventKind =
        | Created = 0
        | Changed = 1
        | Deleted = 2

    type [<AllowNullLiteral>] FileWatcherCallback =
        [<Emit "$0($1...)">] abstract Invoke: fileName: string * eventKind: FileWatcherEventKind -> unit

    type [<AllowNullLiteral>] DirectoryWatcherCallback =
        [<Emit "$0($1...)">] abstract Invoke: fileName: string -> unit

    type [<AllowNullLiteral>] System =
        abstract args: string[] with get, set
        abstract newLine: string with get, set
        abstract useCaseSensitiveFileNames: bool with get, set
        abstract write: s: string -> unit
        abstract writeOutputIsTTY: unit -> bool
        abstract getWidthOfTerminal: unit -> float
        abstract readFile: path: string * ?encoding: string -> string option
        abstract getFileSize: path: string -> float
        abstract writeFile: path: string * data: string * ?writeByteOrderMark: bool -> unit
        abstract watchFile: path: string * callback: FileWatcherCallback * ?pollingInterval: float * ?options: WatchOptions -> FileWatcher
        abstract watchDirectory: path: string * callback: DirectoryWatcherCallback * ?recursive: bool * ?options: WatchOptions -> FileWatcher
        abstract resolvePath: path: string -> string
        abstract fileExists: path: string -> bool
        abstract directoryExists: path: string -> bool
        abstract createDirectory: path: string -> unit
        abstract getExecutingFilePath: unit -> string
        abstract getCurrentDirectory: unit -> string
        abstract getDirectories: path: string -> string[]
        abstract readDirectory: path: string * ?extensions: string[] * ?exclude: string[] * ?``include``: string[] * ?depth: float -> string[]
        abstract getModifiedTime: path: string -> DateTime option
        abstract setModifiedTime: path: string * time: DateTime -> unit
        abstract deleteFile: path: string -> unit
        /// <summary>A good implementation is node.js' <c>crypto.createHash</c>. (<see href="https://nodejs.org/api/crypto.html#crypto_crypto_createhash_algorithm)" /></summary>
        abstract createHash: data: string -> string
        /// <summary>This must be cryptographically secure. Only implement this method using <c>crypto.createHash("sha256")</c>.</summary>
        abstract createSHA256Hash: data: string -> string
        abstract getMemoryUsage: unit -> float
        abstract exit: ?exitCode: float -> unit
        abstract realpath: path: string -> string
        abstract setTimeout: callback: (obj option[] -> unit) * ms: float * [<ParamArray>] args: obj option[] -> obj option
        abstract clearTimeout: timeoutId: obj option -> unit
        abstract clearScreen: unit -> unit
        abstract base64decode: input: string -> string
        abstract base64encode: input: string -> string

    type [<AllowNullLiteral>] FileWatcher =
        abstract close: unit -> unit

    type [<AllowNullLiteral>] ErrorCallback =
        [<Emit "$0($1...)">] abstract Invoke: message: DiagnosticMessage * length: float -> unit

    type [<AllowNullLiteral>] Scanner =
        abstract getStartPos: unit -> float
        abstract getToken: unit -> SyntaxKind
        abstract getTextPos: unit -> float
        abstract getTokenPos: unit -> float
        abstract getTokenText: unit -> string
        abstract getTokenValue: unit -> string
        abstract hasUnicodeEscape: unit -> bool
        abstract hasExtendedUnicodeEscape: unit -> bool
        abstract hasPrecedingLineBreak: unit -> bool
        abstract isIdentifier: unit -> bool
        abstract isReservedWord: unit -> bool
        abstract isUnterminated: unit -> bool
        abstract reScanGreaterToken: unit -> SyntaxKind
        abstract reScanSlashToken: unit -> SyntaxKind
        abstract reScanAsteriskEqualsToken: unit -> SyntaxKind
        abstract reScanTemplateToken: isTaggedTemplate: bool -> SyntaxKind
        abstract reScanTemplateHeadOrNoSubstitutionTemplate: unit -> SyntaxKind
        abstract scanJsxIdentifier: unit -> SyntaxKind
        abstract scanJsxAttributeValue: unit -> SyntaxKind
        abstract reScanJsxAttributeValue: unit -> SyntaxKind
        abstract reScanJsxToken: ?allowMultilineJsxText: bool -> JsxTokenSyntaxKind
        abstract reScanLessThanToken: unit -> SyntaxKind
        abstract reScanHashToken: unit -> SyntaxKind
        abstract reScanQuestionToken: unit -> SyntaxKind
        abstract reScanInvalidIdentifier: unit -> SyntaxKind
        abstract scanJsxToken: unit -> JsxTokenSyntaxKind
        abstract scanJsDocToken: unit -> JSDocSyntaxKind
        abstract scan: unit -> SyntaxKind
        abstract getText: unit -> string
        abstract setText: text: string option * ?start: float * ?length: float -> unit
        abstract setOnError: onError: ErrorCallback option -> unit
        abstract setScriptTarget: scriptTarget: ScriptTarget -> unit
        abstract setLanguageVariant: variant: LanguageVariant -> unit
        abstract setTextPos: textPos: float -> unit
        abstract lookAhead: callback: (unit -> 'T) -> 'T
        abstract scanRange: start: float * length: float * callback: (unit -> 'T) -> 'T
        abstract tryScan: callback: (unit -> 'T) -> 'T

    type [<AllowNullLiteral>] ParameterPropertyDeclaration =
        interface end

    type [<AllowNullLiteral>] DiagnosticReporter =
        [<Emit "$0($1...)">] abstract Invoke: diagnostic: Diagnostic -> unit

    /// Reports config file diagnostics
    type [<AllowNullLiteral>] ConfigFileDiagnosticsReporter =
        /// Reports unrecoverable error when parsing config file
        abstract onUnRecoverableConfigFileDiagnostic: DiagnosticReporter with get, set

    /// Interface extending ParseConfigHost to support ParseConfigFile that reads config file and reports errors
    type [<AllowNullLiteral>] ParseConfigFileHost =
        inherit ParseConfigHost
        inherit ConfigFileDiagnosticsReporter
        abstract getCurrentDirectory: unit -> string

    type [<AllowNullLiteral>] ParsedTsconfig =
        abstract raw: obj option with get, set
        abstract options: CompilerOptions option with get, set
        abstract watchOptions: WatchOptions option with get, set
        abstract typeAcquisition: TypeAcquisition option with get, set
        /// Note that the case of the config path has not yet been normalized, as no files have been imported into the project yet
        abstract extendedConfigPath: string option with get, set

    type [<AllowNullLiteral>] ExtendedConfigCacheEntry =
        abstract extendedResult: TsConfigSourceFile with get, set
        abstract extendedConfig: ParsedTsconfig option with get, set

    type [<AllowNullLiteral>] TypeReferenceDirectiveResolutionCache =
        inherit PerDirectoryResolutionCache<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>
        inherit PackageJsonInfoCache

    type [<AllowNullLiteral>] ModeAwareCache<'T> =
        abstract get: key: string * mode: ModuleKind option -> 'T option
        abstract set: key: string * mode: ModuleKind option * value: 'T -> ModeAwareCache<'T>
        abstract delete: key: string * mode: ModuleKind option -> ModeAwareCache<'T>
        abstract has: key: string * mode: ModuleKind option -> bool
        abstract forEach: cb: ('T -> string -> ModuleKind option -> unit) -> unit
        abstract size: unit -> float

    /// Cached resolutions per containing directory.
    /// This assumes that any module id will have the same resolution for sibling files located in the same folder.
    type [<AllowNullLiteral>] PerDirectoryResolutionCache<'T> =
        abstract getOrCreateCacheForDirectory: directoryName: string * ?redirectedReference: ResolvedProjectReference -> ModeAwareCache<'T>
        abstract clear: unit -> unit
        /// Updates with the current compilerOptions the cache will operate with.
        /// This updates the redirects map as well if needed so module resolutions are cached if they can across the projects
        abstract update: options: CompilerOptions -> unit

    type [<AllowNullLiteral>] ModuleResolutionCache =
        inherit PerDirectoryResolutionCache<ResolvedModuleWithFailedLookupLocations>
        inherit NonRelativeModuleNameResolutionCache
        inherit PackageJsonInfoCache
        abstract getPackageJsonInfoCache: unit -> PackageJsonInfoCache

    /// Stored map from non-relative module name to a table: directory -> result of module lookup in this directory
    /// We support only non-relative module names because resolution of relative module names is usually more deterministic and thus less expensive.
    type [<AllowNullLiteral>] NonRelativeModuleNameResolutionCache =
        inherit PackageJsonInfoCache
        abstract getOrCreateCacheForModuleName: nonRelativeModuleName: string * mode: ModuleKind option * ?redirectedReference: ResolvedProjectReference -> PerModuleNameCache

    type [<AllowNullLiteral>] PackageJsonInfoCache =
        abstract clear: unit -> unit

    type [<AllowNullLiteral>] PerModuleNameCache =
        abstract get: directory: string -> ResolvedModuleWithFailedLookupLocations option
        abstract set: directory: string * result: ResolvedModuleWithFailedLookupLocations -> unit

    type [<AllowNullLiteral>] FormatDiagnosticsHost =
        abstract getCurrentDirectory: unit -> string
        abstract getCanonicalFileName: fileName: string -> string
        abstract getNewLine: unit -> string

    [<Obsolete("")>]
    type [<AllowNullLiteral>] ResolveProjectReferencePathHost =
        abstract fileExists: fileName: string -> bool

    type [<AllowNullLiteral>] EmitOutput =
        abstract outputFiles: OutputFile[] with get, set
        abstract emitSkipped: bool with get, set

    type [<AllowNullLiteral>] OutputFile =
        abstract name: string with get, set
        abstract writeByteOrderMark: bool with get, set
        abstract text: string with get, set

    type AffectedFileResult<'T> =
        {| result: 'T; affected: U2<SourceFile, Program> |} option

    type [<AllowNullLiteral>] BuilderProgramHost =
        /// return true if file names are treated with case sensitivity
        abstract useCaseSensitiveFileNames: unit -> bool
        /// If provided this would be used this hash instead of actual file shape text for detecting changes
        abstract createHash: (string -> string) option with get, set
        /// When emit or emitNextAffectedFile are called without writeFile,
        /// this callback if present would be used to write files
        abstract writeFile: WriteFileCallback option with get, set

    /// Builder to manage the program state changes
    type [<AllowNullLiteral>] BuilderProgram =
        /// Returns current program
        abstract getProgram: unit -> Program
        /// Get compiler options of the program
        abstract getCompilerOptions: unit -> CompilerOptions
        /// Get the source file in the program with file name
        abstract getSourceFile: fileName: string -> SourceFile option
        /// Get a list of files in the program
        abstract getSourceFiles: unit -> SourceFile[]
        /// Get the diagnostics for compiler options
        abstract getOptionsDiagnostics: ?cancellationToken: CancellationToken -> Diagnostic[]
        /// Get the diagnostics that dont belong to any file
        abstract getGlobalDiagnostics: ?cancellationToken: CancellationToken -> Diagnostic[]
        /// Get the diagnostics from config file parsing
        abstract getConfigFileParsingDiagnostics: unit -> Diagnostic[]
        /// Get the syntax diagnostics, for all source files if source file is not supplied
        abstract getSyntacticDiagnostics: ?sourceFile: SourceFile * ?cancellationToken: CancellationToken -> Diagnostic[]
        /// Get the declaration diagnostics, for all source files if source file is not supplied
        abstract getDeclarationDiagnostics: ?sourceFile: SourceFile * ?cancellationToken: CancellationToken -> DiagnosticWithLocation[]
        /// Get all the dependencies of the file
        abstract getAllDependencies: sourceFile: SourceFile -> string[]
        /// Gets the semantic diagnostics from the program corresponding to this state of file (if provided) or whole program
        /// The semantic diagnostics are cached and managed here
        /// Note that it is assumed that when asked about semantic diagnostics through this API,
        /// the file has been taken out of affected files so it is safe to use cache or get from program and cache the diagnostics
        /// In case of SemanticDiagnosticsBuilderProgram if the source file is not provided,
        /// it will iterate through all the affected files, to ensure that cache stays valid and yet provide a way to get all semantic diagnostics
        abstract getSemanticDiagnostics: ?sourceFile: SourceFile * ?cancellationToken: CancellationToken -> Diagnostic[]
        /// Emits the JavaScript and declaration files.
        /// When targetSource file is specified, emits the files corresponding to that source file,
        /// otherwise for the whole program.
        /// In case of EmitAndSemanticDiagnosticsBuilderProgram, when targetSourceFile is specified,
        /// it is assumed that that file is handled from affected file list. If targetSourceFile is not specified,
        /// it will only emit all the affected files instead of whole program
        ///
        /// The first of writeFile if provided, writeFile of BuilderProgramHost if provided, writeFile of compiler host
        /// in that order would be used to write the files
        abstract emit: ?targetSourceFile: SourceFile * ?writeFile: WriteFileCallback * ?cancellationToken: CancellationToken * ?emitOnlyDtsFiles: bool * ?customTransformers: CustomTransformers -> EmitResult
        /// Get the current directory of the program
        abstract getCurrentDirectory: unit -> string

    /// The builder that caches the semantic diagnostics for the program and handles the changed files and affected files
    type [<AllowNullLiteral>] SemanticDiagnosticsBuilderProgram =
        inherit BuilderProgram
        /// Gets the semantic diagnostics from the program for the next affected file and caches it
        /// Returns undefined if the iteration is complete
        abstract getSemanticDiagnosticsOfNextAffectedFile: ?cancellationToken: CancellationToken * ?ignoreSourceFile: (SourceFile -> bool) -> AffectedFileResult<Diagnostic[]>

    /// The builder that can handle the changes in program and iterate through changed file to emit the files
    /// The semantic diagnostics are cached per file and managed by clearing for the changed/affected files
    type [<AllowNullLiteral>] EmitAndSemanticDiagnosticsBuilderProgram =
        inherit SemanticDiagnosticsBuilderProgram
        /// Emits the next affected file's emit result (EmitResult and sourceFiles emitted) or returns undefined if iteration is complete
        /// The first of writeFile if provided, writeFile of BuilderProgramHost if provided, writeFile of compiler host
        /// in that order would be used to write the files
        abstract emitNextAffectedFile: ?writeFile: WriteFileCallback * ?cancellationToken: CancellationToken * ?emitOnlyDtsFiles: bool * ?customTransformers: CustomTransformers -> AffectedFileResult<EmitResult>

    type [<AllowNullLiteral>] ReadBuildProgramHost =
        abstract useCaseSensitiveFileNames: unit -> bool
        abstract getCurrentDirectory: unit -> string
        abstract readFile: fileName: string -> string option

    type [<AllowNullLiteral>] IncrementalProgramOptions<'T> =
        abstract rootNames: string[] with get, set
        abstract options: CompilerOptions with get, set
        abstract configFileParsingDiagnostics: Diagnostic[] option with get, set
        abstract projectReferences: ProjectReference[] option with get, set
        abstract host: CompilerHost option with get, set
        abstract createProgram: CreateProgram<'T> option with get, set

    type [<AllowNullLiteral>] WatchStatusReporter =
        [<Emit "$0($1...)">] abstract Invoke: diagnostic: Diagnostic * newLine: string * options: CompilerOptions * ?errorCount: float -> unit

    /// Create the program with rootNames and options, if they are undefined, oldProgram and new configFile diagnostics create new program
    type [<AllowNullLiteral>] CreateProgram<'T> =
        /// Create the program with rootNames and options, if they are undefined, oldProgram and new configFile diagnostics create new program
        [<Emit "$0($1...)">] abstract Invoke: rootNames: string[] option * options: CompilerOptions option * ?host: CompilerHost * ?oldProgram: 'T * ?configFileParsingDiagnostics: Diagnostic[] * ?projectReferences: ProjectReference[] -> 'T

    /// Host that has watch functionality used in --watch mode
    type [<AllowNullLiteral>] WatchHost =
        /// If provided, called with Diagnostic message that informs about change in watch status
        abstract onWatchStatusChange: diagnostic: Diagnostic * newLine: string * options: CompilerOptions * ?errorCount: float -> unit
        /// Used to watch changes in source files, missing files needed to update the program or config file
        abstract watchFile: path: string * callback: FileWatcherCallback * ?pollingInterval: float * ?options: CompilerOptions -> FileWatcher
        /// Used to watch resolved module's failed lookup locations, config file specs, type roots where auto type reference directives are added
        abstract watchDirectory: path: string * callback: DirectoryWatcherCallback * ?recursive: bool * ?options: CompilerOptions -> FileWatcher
        /// If provided, will be used to set delayed compilation, so that multiple changes in short span are compiled together
        abstract setTimeout: callback: (obj option[] -> unit) * ms: float * [<ParamArray>] args: obj option[] -> obj option
        /// If provided, will be used to reset existing delayed compilation
        abstract clearTimeout: timeoutId: obj option -> unit

    type [<AllowNullLiteral>] ProgramHost<'T> =
        /// Used to create the program when need for program creation or recreation detected
        abstract createProgram: CreateProgram<'T> with get, set
        abstract useCaseSensitiveFileNames: unit -> bool
        abstract getNewLine: unit -> string
        abstract getCurrentDirectory: unit -> string
        abstract getDefaultLibFileName: options: CompilerOptions -> string
        abstract getDefaultLibLocation: unit -> string
        abstract createHash: data: string -> string
        /// Use to check file presence for source files and
        /// if resolveModuleNames is not provided (complier is in charge of module resolution) then module files as well
        abstract fileExists: path: string -> bool
        /// Use to read file text for source files and
        /// if resolveModuleNames is not provided (complier is in charge of module resolution) then module files as well
        abstract readFile: path: string * ?encoding: string -> string option
        /// If provided, used for module resolution as well as to handle directory structure
        abstract directoryExists: path: string -> bool
        /// If provided, used in resolutions as well as handling directory structure
        abstract getDirectories: path: string -> string[]
        /// If provided, used to cache and handle directory structure modifications
        abstract readDirectory: path: string * ?extensions: string[] * ?exclude: string[] * ?``include``: string[] * ?depth: float -> string[]
        /// Symbol links resolution
        abstract realpath: path: string -> string
        /// If provided would be used to write log about compilation
        abstract trace: s: string -> unit
        /// If provided is used to get the environment variable
        abstract getEnvironmentVariable: name: string -> string option
        /// If provided, used to resolve the module names, otherwise typescript's default module resolution
        abstract resolveModuleNames: moduleNames: string[] * containingFile: string * reusedNames: string[] option * redirectedReference: ResolvedProjectReference option * options: CompilerOptions * ?containingSourceFile: SourceFile -> ResolvedModule option[]
        /// If provided, used to resolve type reference directives, otherwise typescript's default resolution
        abstract resolveTypeReferenceDirectives: typeReferenceDirectiveNames: string[] * containingFile: string * redirectedReference: ResolvedProjectReference option * options: CompilerOptions -> ResolvedTypeReferenceDirective option[]

    type [<AllowNullLiteral>] WatchCompilerHost<'T> =
        inherit ProgramHost<'T>
        inherit WatchHost
        /// Instead of using output d.ts file from project reference, use its source file
        abstract useSourceOfProjectReferenceRedirect: unit -> bool
        /// If provided, use this method to get parsed command lines for referenced projects
        abstract getParsedCommandLine: fileName: string -> ParsedCommandLine option
        /// If provided, callback to invoke after every new program creation
        abstract afterProgramCreate: program: 'T -> unit

    /// Host to create watch with root files and options
    type [<AllowNullLiteral>] WatchCompilerHostOfFilesAndCompilerOptions<'T> =
        inherit WatchCompilerHost<'T>
        /// root files to use to generate program
        abstract rootFiles: string[] with get, set
        /// Compiler options
        abstract options: CompilerOptions with get, set
        abstract watchOptions: WatchOptions option with get, set
        /// Project References
        abstract projectReferences: ProjectReference[] option with get, set

    /// Host to create watch with config file
    type [<AllowNullLiteral>] WatchCompilerHostOfConfigFile<'T> =
        inherit WatchCompilerHost<'T>
        inherit ConfigFileDiagnosticsReporter
        /// Name of the config file to compile
        abstract configFileName: string with get, set
        /// Options to extend
        abstract optionsToExtend: CompilerOptions option with get, set
        abstract watchOptionsToExtend: WatchOptions option with get, set
        abstract extraFileExtensions: FileExtensionInfo[] option with get, set
        /// Used to generate source file names from the config file and its include, exclude, files rules
        /// and also to cache the directory stucture
        abstract readDirectory: path: string * ?extensions: string[] * ?exclude: string[] * ?``include``: string[] * ?depth: float -> string[]

    type [<AllowNullLiteral>] Watch<'T> =
        /// Synchronize with host and get updated program
        abstract getProgram: unit -> 'T
        /// Closes the watch
        abstract close: unit -> unit

    /// Creates the watch what generates program using the config file
    type [<AllowNullLiteral>] WatchOfConfigFile<'T> =
        inherit Watch<'T>

    /// Creates the watch that generates program using the root files and compiler options
    type [<AllowNullLiteral>] WatchOfFilesAndCompilerOptions<'T> =
        inherit Watch<'T>
        /// Updates the root files in the program, only if this is not config file compilation
        abstract updateRootFileNames: fileNames: string[] -> unit

    type [<AllowNullLiteral>] BuildOptions =
        abstract dry: bool option with get, set
        abstract force: bool option with get, set
        abstract verbose: bool option with get, set
        abstract incremental: bool option with get, set
        abstract assumeChangesOnlyAffectDirectDependencies: bool option with get, set
        abstract traceResolution: bool option with get, set
        [<EmitIndexer>] abstract Item: option: string -> CompilerOptionsValue option with get, set

    type [<AllowNullLiteral>] ReportEmitErrorSummary =
        [<Emit "$0($1...)">] abstract Invoke: errorCount: float * filesInError: ReportFileInError option[] -> unit

    type [<AllowNullLiteral>] ReportFileInError =
        abstract fileName: string with get, set
        abstract line: float with get, set

    type [<AllowNullLiteral>] SolutionBuilderHostBase<'T> =
        inherit ProgramHost<'T>
        abstract createDirectory: path: string -> unit
        /// Should provide create directory and writeFile if done of invalidatedProjects is not invoked with
        /// writeFileCallback
        abstract writeFile: path: string * data: string * ?writeByteOrderMark: bool -> unit
        abstract getCustomTransformers: (string -> CustomTransformers option) option with get, set
        abstract getModifiedTime: fileName: string -> DateTime option
        abstract setModifiedTime: fileName: string * date: DateTime -> unit
        abstract deleteFile: fileName: string -> unit
        abstract getParsedCommandLine: fileName: string -> ParsedCommandLine option
        abstract reportDiagnostic: DiagnosticReporter with get, set
        abstract reportSolutionBuilderStatus: DiagnosticReporter with get, set
        abstract afterProgramEmitAndDiagnostics: program: 'T -> unit

    type [<AllowNullLiteral>] SolutionBuilderHost<'T> =
        inherit SolutionBuilderHostBase<'T>
        abstract reportErrorSummary: ReportEmitErrorSummary option with get, set

    type [<AllowNullLiteral>] SolutionBuilderWithWatchHost<'T> =
        inherit SolutionBuilderHostBase<'T>
        inherit WatchHost

    type [<AllowNullLiteral>] SolutionBuilder<'T> =
        abstract build: ?project: string * ?cancellationToken: CancellationToken * ?writeFile: WriteFileCallback * ?getCustomTransformers: (string -> CustomTransformers) -> ExitStatus
        abstract clean: ?project: string -> ExitStatus
        abstract buildReferences: project: string * ?cancellationToken: CancellationToken * ?writeFile: WriteFileCallback * ?getCustomTransformers: (string -> CustomTransformers) -> ExitStatus
        abstract cleanReferences: ?project: string -> ExitStatus
        abstract getNextInvalidatedProject: ?cancellationToken: CancellationToken -> InvalidatedProject<'T> option

    type [<RequireQualifiedAccess>] InvalidatedProjectKind =
        | Build = 0
        | UpdateBundle = 1
        | UpdateOutputFileStamps = 2

    type [<AllowNullLiteral>] InvalidatedProjectBase =
        abstract kind: InvalidatedProjectKind
        abstract project: ResolvedConfigFileName
        /// To dispose this project and ensure that all the necessary actions are taken and state is updated accordingly
        abstract ``done``: ?cancellationToken: CancellationToken * ?writeFile: WriteFileCallback * ?customTransformers: CustomTransformers -> ExitStatus
        abstract getCompilerOptions: unit -> CompilerOptions
        abstract getCurrentDirectory: unit -> string

    type [<AllowNullLiteral>] UpdateOutputFileStampsProject =
        inherit InvalidatedProjectBase
        abstract kind: InvalidatedProjectKind
        abstract updateOutputFileStatmps: unit -> unit

    type [<AllowNullLiteral>] BuildInvalidedProject<'T> =
        inherit InvalidatedProjectBase
        abstract kind: InvalidatedProjectKind
        abstract getBuilderProgram: unit -> 'T option
        abstract getProgram: unit -> Program option
        abstract getSourceFile: fileName: string -> SourceFile option
        abstract getSourceFiles: unit -> SourceFile[]
        abstract getOptionsDiagnostics: ?cancellationToken: CancellationToken -> Diagnostic[]
        abstract getGlobalDiagnostics: ?cancellationToken: CancellationToken -> Diagnostic[]
        abstract getConfigFileParsingDiagnostics: unit -> Diagnostic[]
        abstract getSyntacticDiagnostics: ?sourceFile: SourceFile * ?cancellationToken: CancellationToken -> Diagnostic[]
        abstract getAllDependencies: sourceFile: SourceFile -> string[]
        abstract getSemanticDiagnostics: ?sourceFile: SourceFile * ?cancellationToken: CancellationToken -> Diagnostic[]
        abstract getSemanticDiagnosticsOfNextAffectedFile: ?cancellationToken: CancellationToken * ?ignoreSourceFile: (SourceFile -> bool) -> AffectedFileResult<Diagnostic[]>
        abstract emit: ?targetSourceFile: SourceFile * ?writeFile: WriteFileCallback * ?cancellationToken: CancellationToken * ?emitOnlyDtsFiles: bool * ?customTransformers: CustomTransformers -> EmitResult option

    type [<AllowNullLiteral>] UpdateBundleProject<'T> =
        inherit InvalidatedProjectBase
        abstract kind: InvalidatedProjectKind
        abstract emit: ?writeFile: WriteFileCallback * ?customTransformers: CustomTransformers -> U2<EmitResult, BuildInvalidedProject<'T>> option

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] InvalidatedProject<'T> =
        | [<CompiledValue 0>] BuildInvalidedProject of BuildInvalidedProject<'T>
        | [<CompiledValue 1>] UpdateBundleProject of UpdateBundleProject<'T>
        | [<CompiledValue 2>] UpdateOutputFileStampsProject of UpdateOutputFileStampsProject

    module Server =

        type ActionSet =
            string

        type ActionInvalidate =
            string

        type ActionPackageInstalled =
            string

        type EventTypesRegistry =
            string

        type EventBeginInstallTypes =
            string

        type EventEndInstallTypes =
            string

        type EventInitializationFailed =
            string

        type [<AllowNullLiteral>] TypingInstallerResponse =
            abstract kind: U7<ActionSet, ActionInvalidate, EventTypesRegistry, ActionPackageInstalled, EventBeginInstallTypes, EventEndInstallTypes, EventInitializationFailed>

        type [<AllowNullLiteral>] TypingInstallerRequestWithProjectName =
            abstract projectName: string

        type [<AllowNullLiteral>] DiscoverTypings =
            inherit TypingInstallerRequestWithProjectName
            abstract fileNames: string[]
            abstract projectRootPath: Path
            abstract compilerOptions: CompilerOptions
            abstract watchOptions: WatchOptions option
            abstract typeAcquisition: TypeAcquisition
            abstract unresolvedImports: SortedReadonlyArray<string>
            abstract cachePath: string option
            abstract kind: string

        type [<AllowNullLiteral>] CloseProject =
            inherit TypingInstallerRequestWithProjectName
            abstract kind: string

        type [<AllowNullLiteral>] TypesRegistryRequest =
            abstract kind: string

        type [<AllowNullLiteral>] InstallPackageRequest =
            inherit TypingInstallerRequestWithProjectName
            abstract kind: string
            abstract fileName: Path
            abstract packageName: string
            abstract projectRootPath: Path

        type [<AllowNullLiteral>] PackageInstalledResponse =
            inherit ProjectResponse
            abstract kind: ActionPackageInstalled
            abstract success: bool
            abstract message: string

        type [<AllowNullLiteral>] InitializationFailedResponse =
            inherit TypingInstallerResponse
            abstract kind: EventInitializationFailed
            abstract message: string
            abstract stack: string option

        type [<AllowNullLiteral>] ProjectResponse =
            inherit TypingInstallerResponse
            abstract projectName: string

        type [<AllowNullLiteral>] InvalidateCachedTypings =
            inherit ProjectResponse
            abstract kind: ActionInvalidate

        type [<AllowNullLiteral>] InstallTypes =
            inherit ProjectResponse
            abstract kind: U2<EventBeginInstallTypes, EventEndInstallTypes>
            abstract eventId: float
            abstract typingsInstallerVersion: string
            abstract packagesToInstall: string[]

        type [<AllowNullLiteral>] BeginInstallTypes =
            inherit InstallTypes
            abstract kind: EventBeginInstallTypes

        type [<AllowNullLiteral>] EndInstallTypes =
            inherit InstallTypes
            abstract kind: EventEndInstallTypes
            abstract installSuccess: bool

        type [<AllowNullLiteral>] SetTypings =
            inherit ProjectResponse
            abstract typeAcquisition: TypeAcquisition
            abstract compilerOptions: CompilerOptions
            abstract typings: string[]
            abstract unresolvedImports: SortedReadonlyArray<string>
            abstract kind: ActionSet

    type [<AllowNullLiteral>] SourceFileLike =
        abstract getLineAndCharacterOfPosition: pos: float -> LineAndCharacter

    /// Represents an immutable snapshot of a script at a specified time.Once acquired, the
    /// snapshot is observably immutable. i.e. the same calls with the same parameters will return
    /// the same values.
    type [<AllowNullLiteral>] IScriptSnapshot =
        /// Gets a portion of the script snapshot specified by [start, end).
        abstract getText: start: float * ``end``: float -> string
        /// Gets the length of this script snapshot.
        abstract getLength: unit -> float
        /// Gets the TextChangeRange that describe how the text changed between this text and
        /// an older version.  This information is used by the incremental parser to determine
        /// what sections of the script need to be re-parsed.  'undefined' can be returned if the
        /// change range cannot be determined.  However, in that case, incremental parsing will
        /// not happen and the entire document will be re - parsed.
        abstract getChangeRange: oldSnapshot: IScriptSnapshot -> TextChangeRange option
        /// Releases all resources held by this script snapshot
        abstract dispose: unit -> unit

    module ScriptSnapshot =

        type [<AllowNullLiteral>] IExports =
            abstract fromString: text: string -> IScriptSnapshot

    type [<AllowNullLiteral>] PreProcessedFileInfo =
        abstract referencedFiles: FileReference[] with get, set
        abstract typeReferenceDirectives: FileReference[] with get, set
        abstract libReferenceDirectives: FileReference[] with get, set
        abstract importedFiles: FileReference[] with get, set
        abstract ambientExternalModules: string[] option with get, set
        abstract isLibFile: bool with get, set

    type [<AllowNullLiteral>] HostCancellationToken =
        abstract isCancellationRequested: unit -> bool

    type [<AllowNullLiteral>] InstallPackageOptions =
        abstract fileName: Path with get, set
        abstract packageName: string with get, set

    type [<AllowNullLiteral>] PerformanceEvent =
        abstract kind: PerformanceEventKind with get, set
        abstract durationMs: float with get, set

    type [<RequireQualifiedAccess>] LanguageServiceMode =
        | Semantic = 0
        | PartialSemantic = 1
        | Syntactic = 2

    type [<AllowNullLiteral>] IncompleteCompletionsCache =
        abstract get: unit -> CompletionInfo option
        abstract set: response: CompletionInfo -> unit
        abstract clear: unit -> unit

    type [<AllowNullLiteral>] LanguageServiceHost =
        inherit GetEffectiveTypeRootsHost
        abstract getCompilationSettings: unit -> CompilerOptions
        abstract getNewLine: unit -> string
        abstract getProjectVersion: unit -> string
        abstract getScriptFileNames: unit -> string[]
        abstract getScriptKind: fileName: string -> ScriptKind
        abstract getScriptVersion: fileName: string -> string
        abstract getScriptSnapshot: fileName: string -> IScriptSnapshot option
        abstract getProjectReferences: unit -> ProjectReference[] option
        abstract getLocalizedDiagnosticMessages: unit -> obj option
        abstract getCancellationToken: unit -> HostCancellationToken
        abstract getCurrentDirectory: unit -> string
        abstract getDefaultLibFileName: options: CompilerOptions -> string
        abstract log: s: string -> unit
        abstract trace: s: string -> unit
        abstract error: s: string -> unit
        abstract useCaseSensitiveFileNames: unit -> bool
        abstract readDirectory: path: string * ?extensions: string[] * ?exclude: string[] * ?``include``: string[] * ?depth: float -> string[]
        abstract readFile: path: string * ?encoding: string -> string option
        abstract realpath: path: string -> string
        abstract fileExists: path: string -> bool
        abstract getTypeRootsVersion: unit -> float
        abstract resolveModuleNames: moduleNames: string[] * containingFile: string * reusedNames: string[] option * redirectedReference: ResolvedProjectReference option * options: CompilerOptions * ?containingSourceFile: SourceFile -> ResolvedModule option[]
        abstract getResolvedModuleWithFailedLookupLocationsFromCache: modulename: string * containingFile: string * ?resolutionMode: ModuleKind -> ResolvedModuleWithFailedLookupLocations option
        abstract resolveTypeReferenceDirectives: typeDirectiveNames: string[] * containingFile: string * redirectedReference: ResolvedProjectReference option * options: CompilerOptions -> ResolvedTypeReferenceDirective option[]
        abstract getDirectories: directoryName: string -> string[]
        /// Gets a set of custom transformers to use during emit.
        abstract getCustomTransformers: unit -> CustomTransformers option
        abstract isKnownTypesPackageName: name: string -> bool
        abstract installPackage: options: InstallPackageOptions -> Promise<ApplyCodeActionCommandResult>
        abstract writeFile: fileName: string * content: string -> unit
        abstract getParsedCommandLine: fileName: string -> ParsedCommandLine option

    type [<AllowNullLiteral>] WithMetadata<'T> =
        interface end

    type [<StringEnum>] [<RequireQualifiedAccess>] SemanticClassificationFormat =
        | Original
        | [<CompiledName "2020">] TwentyTwenty

    type [<AllowNullLiteral>] LanguageService =
        /// This is used as a part of restarting the language service.
        abstract cleanupSemanticCache: unit -> unit
        /// <summary>
        /// Gets errors indicating invalid syntax in a file.
        ///
        /// In English, "this cdeo have, erorrs" is syntactically invalid because it has typos,
        /// grammatical errors, and misplaced punctuation. Likewise, examples of syntax
        /// errors in TypeScript are missing parentheses in an <c>if</c> statement, mismatched
        /// curly braces, and using a reserved keyword as a variable name.
        ///
        /// These diagnostics are inexpensive to compute and don't require knowledge of
        /// other files. Note that a non-empty result increases the likelihood of false positives
        /// from <c>getSemanticDiagnostics</c>.
        ///
        /// While these represent the majority of syntax-related diagnostics, there are some
        /// that require the type system, which will be present in <c>getSemanticDiagnostics</c>.
        /// </summary>
        /// <param name="fileName">A path to the file you want syntactic diagnostics for</param>
        abstract getSyntacticDiagnostics: fileName: string -> DiagnosticWithLocation[]
        /// <summary>
        /// Gets warnings or errors indicating type system issues in a given file.
        /// Requesting semantic diagnostics may start up the type system and
        /// run deferred work, so the first call may take longer than subsequent calls.
        ///
        /// Unlike the other get*Diagnostics functions, these diagnostics can potentially not
        /// include a reference to a source file. Specifically, the first time this is called,
        /// it will return global diagnostics with no associated location.
        ///
        /// To contrast the differences between semantic and syntactic diagnostics, consider the
        /// sentence: "The sun is green." is syntactically correct; those are real English words with
        /// correct sentence structure. However, it is semantically invalid, because it is not true.
        /// </summary>
        /// <param name="fileName">A path to the file you want semantic diagnostics for</param>
        abstract getSemanticDiagnostics: fileName: string -> Diagnostic[]
        /// <summary>
        /// Gets suggestion diagnostics for a specific file. These diagnostics tend to
        /// proactively suggest refactors, as opposed to diagnostics that indicate
        /// potentially incorrect runtime behavior.
        /// </summary>
        /// <param name="fileName">A path to the file you want semantic diagnostics for</param>
        abstract getSuggestionDiagnostics: fileName: string -> DiagnosticWithLocation[]
        /// Gets global diagnostics related to the program configuration and compiler options.
        abstract getCompilerOptionsDiagnostics: unit -> Diagnostic[]
        [<Obsolete("Use getEncodedSyntacticClassifications instead.")>]
        abstract getSyntacticClassifications: fileName: string * span: TextSpan -> ClassifiedSpan[]
        abstract getSyntacticClassifications: fileName: string * span: TextSpan * format: SemanticClassificationFormat -> U2<ClassifiedSpan[], ClassifiedSpan2020[]>
        [<Obsolete("Use getEncodedSemanticClassifications instead.")>]
        abstract getSemanticClassifications: fileName: string * span: TextSpan -> ClassifiedSpan[]
        abstract getSemanticClassifications: fileName: string * span: TextSpan * format: SemanticClassificationFormat -> U2<ClassifiedSpan[], ClassifiedSpan2020[]>
        /// Encoded as triples of [start, length, ClassificationType].
        abstract getEncodedSyntacticClassifications: fileName: string * span: TextSpan -> Classifications
        /// <summary>
        /// Gets semantic highlights information for a particular file. Has two formats, an older
        /// version used by VS and a format used by VS Code.
        /// </summary>
        /// <param name="fileName">The path to the file</param>
        /// <param name="position">A text span to return results within</param>
        /// <param name="format">Which format to use, defaults to "original"</param>
        /// <returns>a number array encoded as triples of [start, length, ClassificationType, ...].</returns>
        abstract getEncodedSemanticClassifications: fileName: string * span: TextSpan * ?format: SemanticClassificationFormat -> Classifications
        /// <summary>Gets completion entries at a particular position in a file.</summary>
        /// <param name="fileName">The path to the file</param>
        /// <param name="position">A zero-based index of the character where you want the entries</param>
        /// <param name="options">
        /// An object describing how the request was triggered and what kinds
        /// of code actions can be returned with the completions.
        /// </param>
        /// <param name="formattingSettings">settings needed for calling formatting functions.</param>
        abstract getCompletionsAtPosition: fileName: string * position: float * options: GetCompletionsAtPositionOptions option * ?formattingSettings: FormatCodeSettings -> WithMetadata<CompletionInfo> option
        /// <summary>Gets the extended details for a completion entry retrieved from <c>getCompletionsAtPosition</c>.</summary>
        /// <param name="fileName">The path to the file</param>
        /// <param name="position">A zero based index of the character where you want the entries</param>
        /// <param name="entryName">The <c>name</c> from an existing completion which came from <c>getCompletionsAtPosition</c></param>
        /// <param name="formatOptions">How should code samples in the completions be formatted, can be undefined for backwards compatibility</param>
        /// <param name="source"><c>source</c> property from the completion entry</param>
        /// <param name="preferences">User settings, can be undefined for backwards compatibility</param>
        /// <param name="data"><c>data</c> property from the completion entry</param>
        abstract getCompletionEntryDetails: fileName: string * position: float * entryName: string * formatOptions: U2<FormatCodeOptions, FormatCodeSettings> option * source: string option * preferences: UserPreferences option * data: CompletionEntryData option -> CompletionEntryDetails option
        abstract getCompletionEntrySymbol: fileName: string * position: float * name: string * source: string option -> Symbol option
        /// <summary>
        /// Gets semantic information about the identifier at a particular position in a
        /// file. Quick info is what you typically see when you hover in an editor.
        /// </summary>
        /// <param name="fileName">The path to the file</param>
        /// <param name="position">A zero-based index of the character where you want the quick info</param>
        abstract getQuickInfoAtPosition: fileName: string * position: float -> QuickInfo option
        abstract getNameOrDottedNameSpan: fileName: string * startPos: float * endPos: float -> TextSpan option
        abstract getBreakpointStatementAtPosition: fileName: string * position: float -> TextSpan option
        abstract getSignatureHelpItems: fileName: string * position: float * options: SignatureHelpItemsOptions option -> SignatureHelpItems option
        abstract getRenameInfo: fileName: string * position: float * ?options: RenameInfoOptions -> RenameInfo
        abstract findRenameLocations: fileName: string * position: float * findInStrings: bool * findInComments: bool * ?providePrefixAndSuffixTextForRename: bool -> RenameLocation[] option
        abstract getSmartSelectionRange: fileName: string * position: float -> SelectionRange
        abstract getDefinitionAtPosition: fileName: string * position: float -> DefinitionInfo[] option
        abstract getDefinitionAndBoundSpan: fileName: string * position: float -> DefinitionInfoAndBoundSpan option
        abstract getTypeDefinitionAtPosition: fileName: string * position: float -> DefinitionInfo[] option
        abstract getImplementationAtPosition: fileName: string * position: float -> ImplementationLocation[] option
        abstract getReferencesAtPosition: fileName: string * position: float -> ReferenceEntry[] option
        abstract findReferences: fileName: string * position: float -> ReferencedSymbol[] option
        abstract getDocumentHighlights: fileName: string * position: float * filesToSearch: string[] -> DocumentHighlights[] option
        abstract getFileReferences: fileName: string -> ReferenceEntry[]
        [<Obsolete("")>]
        abstract getOccurrencesAtPosition: fileName: string * position: float -> ReferenceEntry[] option
        abstract getNavigateToItems: searchValue: string * ?maxResultCount: float * ?fileName: string * ?excludeDtsFiles: bool -> NavigateToItem[]
        abstract getNavigationBarItems: fileName: string -> NavigationBarItem[]
        abstract getNavigationTree: fileName: string -> NavigationTree
        abstract prepareCallHierarchy: fileName: string * position: float -> U2<CallHierarchyItem, CallHierarchyItem[]> option
        abstract provideCallHierarchyIncomingCalls: fileName: string * position: float -> CallHierarchyIncomingCall[]
        abstract provideCallHierarchyOutgoingCalls: fileName: string * position: float -> CallHierarchyOutgoingCall[]
        abstract provideInlayHints: fileName: string * span: TextSpan * preferences: UserPreferences option -> InlayHint[]
        abstract getOutliningSpans: fileName: string -> OutliningSpan[]
        abstract getTodoComments: fileName: string * descriptors: TodoCommentDescriptor[] -> TodoComment[]
        abstract getBraceMatchingAtPosition: fileName: string * position: float -> TextSpan[]
        abstract getIndentationAtPosition: fileName: string * position: float * options: U2<EditorOptions, EditorSettings> -> float
        abstract getFormattingEditsForRange: fileName: string * start: float * ``end``: float * options: U2<FormatCodeOptions, FormatCodeSettings> -> TextChange[]
        abstract getFormattingEditsForDocument: fileName: string * options: U2<FormatCodeOptions, FormatCodeSettings> -> TextChange[]
        abstract getFormattingEditsAfterKeystroke: fileName: string * position: float * key: string * options: U2<FormatCodeOptions, FormatCodeSettings> -> TextChange[]
        abstract getDocCommentTemplateAtPosition: fileName: string * position: float * ?options: DocCommentTemplateOptions -> TextInsertion option
        abstract isValidBraceCompletionAtPosition: fileName: string * position: float * openingBrace: float -> bool
        /// <summary>
        /// This will return a defined result if the position is after the <c>&gt;</c> of the opening tag, or somewhere in the text, of a JSXElement with no closing tag.
        /// Editors should call this after <c>&gt;</c> is typed.
        /// </summary>
        abstract getJsxClosingTagAtPosition: fileName: string * position: float -> JsxClosingTagInfo option
        abstract getSpanOfEnclosingComment: fileName: string * position: float * onlyMultiLine: bool -> TextSpan option
        abstract toLineColumnOffset: fileName: string * position: float -> LineAndCharacter
        abstract getCodeFixesAtPosition: fileName: string * start: float * ``end``: float * errorCodes: float[] * formatOptions: FormatCodeSettings * preferences: UserPreferences -> CodeFixAction[]
        abstract getCombinedCodeFix: scope: CombinedCodeFixScope * fixId: LanguageServiceGetCombinedCodeFixFixId * formatOptions: FormatCodeSettings * preferences: UserPreferences -> CombinedCodeActions
        abstract applyCodeActionCommand: action: CodeActionCommand * ?formatSettings: FormatCodeSettings -> Promise<ApplyCodeActionCommandResult>
        abstract applyCodeActionCommand: action: CodeActionCommand[] * ?formatSettings: FormatCodeSettings -> Promise<ApplyCodeActionCommandResult[]>
        abstract applyCodeActionCommand: action: U2<CodeActionCommand, CodeActionCommand[]> * ?formatSettings: FormatCodeSettings -> Promise<U2<ApplyCodeActionCommandResult, ApplyCodeActionCommandResult[]>>
        [<Obsolete("`fileName` will be ignored")>]
        abstract applyCodeActionCommand: fileName: string * action: CodeActionCommand -> Promise<ApplyCodeActionCommandResult>
        [<Obsolete("`fileName` will be ignored")>]
        abstract applyCodeActionCommand: fileName: string * action: CodeActionCommand[] -> Promise<ApplyCodeActionCommandResult[]>
        [<Obsolete("`fileName` will be ignored")>]
        abstract applyCodeActionCommand: fileName: string * action: U2<CodeActionCommand, CodeActionCommand[]> -> Promise<U2<ApplyCodeActionCommandResult, ApplyCodeActionCommandResult[]>>
        abstract getApplicableRefactors: fileName: string * positionOrRange: U2<float, TextRange> * preferences: UserPreferences option * ?triggerReason: RefactorTriggerReason * ?kind: string -> ApplicableRefactorInfo[]
        abstract getEditsForRefactor: fileName: string * formatOptions: FormatCodeSettings * positionOrRange: U2<float, TextRange> * refactorName: string * actionName: string * preferences: UserPreferences option -> RefactorEditInfo option
        abstract organizeImports: args: OrganizeImportsArgs * formatOptions: FormatCodeSettings * preferences: UserPreferences option -> FileTextChanges[]
        abstract getEditsForFileRename: oldFilePath: string * newFilePath: string * formatOptions: FormatCodeSettings * preferences: UserPreferences option -> FileTextChanges[]
        abstract getEmitOutput: fileName: string * ?emitOnlyDtsFiles: bool * ?forceDtsEmit: bool -> EmitOutput
        abstract getProgram: unit -> Program option
        abstract toggleLineComment: fileName: string * textRange: TextRange -> TextChange[]
        abstract toggleMultilineComment: fileName: string * textRange: TextRange -> TextChange[]
        abstract commentSelection: fileName: string * textRange: TextRange -> TextChange[]
        abstract uncommentSelection: fileName: string * textRange: TextRange -> TextChange[]
        abstract dispose: unit -> unit

    type [<AllowNullLiteral>] LanguageServiceGetCombinedCodeFixFixId =
        interface end

    type [<AllowNullLiteral>] JsxClosingTagInfo =
        abstract newText: string

    type [<AllowNullLiteral>] CombinedCodeFixScope =
        abstract ``type``: string with get, set
        abstract fileName: string with get, set

    type [<AllowNullLiteral>] OrganizeImportsArgs =
        inherit CombinedCodeFixScope
        abstract skipDestructiveCodeActions: bool option with get, set

    type [<StringEnum>] [<RequireQualifiedAccess>] CompletionsTriggerCharacter =
        | [<CompiledName ".">] Dot
        | [<CompiledName "\"">] DoubleQuote
        | [<CompiledName "'">] SingleQuote
        | [<CompiledName "`">] BackQuote
        | [<CompiledName "/">] Slash
        | [<CompiledName "@">] At
        | [<CompiledName "<">] LessThan
        | [<CompiledName "#">] Sharp
        | [<CompiledName " ">] Empty

    type [<RequireQualifiedAccess>] CompletionTriggerKind =
        /// Completion was triggered by typing an identifier, manual invocation (e.g Ctrl+Space) or via API.
        | Invoked = 1
        /// Completion was triggered by a trigger character.
        | TriggerCharacter = 2
        /// Completion was re-triggered as the current completion list is incomplete.
        | TriggerForIncompleteCompletions = 3

    type [<AllowNullLiteral>] GetCompletionsAtPositionOptions =
        inherit UserPreferences
        /// If the editor is asking for completions because a certain character was typed
        /// (as opposed to when the user explicitly requested them) this should be set.
        abstract triggerCharacter: CompletionsTriggerCharacter option with get, set
        abstract triggerKind: CompletionTriggerKind option with get, set
        [<Obsolete("Use includeCompletionsForModuleExports")>]
        abstract includeExternalModuleExports: bool option with get, set
        [<Obsolete("Use includeCompletionsWithInsertText")>]
        abstract includeInsertTextCompletions: bool option with get, set

    type [<AllowNullLiteral>] InlayHintsOptions =
        inherit UserPreferences
        abstract includeInlayParameterNameHints: InlayHintsOptionsIncludeInlayParameterNameHints option
        abstract includeInlayParameterNameHintsWhenArgumentMatchesName: bool option
        abstract includeInlayFunctionParameterTypeHints: bool option
        abstract includeInlayVariableTypeHints: bool option
        abstract includeInlayPropertyDeclarationTypeHints: bool option
        abstract includeInlayFunctionLikeReturnTypeHints: bool option
        abstract includeInlayEnumMemberValueHints: bool option

    type [<StringEnum>] [<RequireQualifiedAccess>] SignatureHelpTriggerCharacter =
        | [<CompiledName ",">] Comma
        | [<CompiledName "(">] LeftParen
        | [<CompiledName "<">] LessThan

    type SignatureHelpRetriggerCharacter =
        U2<SignatureHelpTriggerCharacter, string>

    type [<AllowNullLiteral>] SignatureHelpItemsOptions =
        abstract triggerReason: SignatureHelpTriggerReason option with get, set

    type [<TypeScriptTaggedUnion("kind")>] [<RequireQualifiedAccess>] SignatureHelpTriggerReason =
        | [<CompiledName "characterTyped">] SignatureHelpCharacterTypedReason of SignatureHelpCharacterTypedReason
        | [<CompiledName "invoked">] SignatureHelpInvokedReason of SignatureHelpInvokedReason
        | [<CompiledName "retrigger">] SignatureHelpRetriggeredReason of SignatureHelpRetriggeredReason

    /// Signals that the user manually requested signature help.
    /// The language service will unconditionally attempt to provide a result.
    type [<AllowNullLiteral>] SignatureHelpInvokedReason =
        abstract kind: string with get, set
        abstract triggerCharacter: obj option with get, set

    /// Signals that the signature help request came from a user typing a character.
    /// Depending on the character and the syntactic context, the request may or may not be served a result.
    type [<AllowNullLiteral>] SignatureHelpCharacterTypedReason =
        abstract kind: string with get, set
        /// Character that was responsible for triggering signature help.
        abstract triggerCharacter: SignatureHelpTriggerCharacter with get, set

    /// <summary>
    /// Signals that this signature help request came from typing a character or moving the cursor.
    /// This should only occur if a signature help session was already active and the editor needs to see if it should adjust.
    /// The language service will unconditionally attempt to provide a result.
    /// <c>triggerCharacter</c> can be <c>undefined</c> for a retrigger caused by a cursor move.
    /// </summary>
    type [<AllowNullLiteral>] SignatureHelpRetriggeredReason =
        abstract kind: string with get, set
        /// Character that was responsible for triggering signature help.
        abstract triggerCharacter: SignatureHelpRetriggerCharacter option with get, set

    type [<AllowNullLiteral>] ApplyCodeActionCommandResult =
        abstract successMessage: string with get, set

    type [<AllowNullLiteral>] Classifications =
        abstract spans: float[] with get, set
        abstract endOfLineState: EndOfLineState with get, set

    type [<AllowNullLiteral>] ClassifiedSpan =
        abstract textSpan: TextSpan with get, set
        abstract classificationType: ClassificationTypeNames with get, set

    type [<AllowNullLiteral>] ClassifiedSpan2020 =
        abstract textSpan: TextSpan with get, set
        abstract classificationType: float with get, set

    /// <summary>
    /// Navigation bar interface designed for visual studio's dual-column layout.
    /// This does not form a proper tree.
    /// The navbar is returned as a list of top-level items, each of which has a list of child items.
    /// Child items always have an empty array for their <c>childItems</c>.
    /// </summary>
    type [<AllowNullLiteral>] NavigationBarItem =
        abstract text: string with get, set
        abstract kind: ScriptElementKind with get, set
        abstract kindModifiers: string with get, set
        abstract spans: TextSpan[] with get, set
        abstract childItems: NavigationBarItem[] with get, set
        abstract indent: float with get, set
        abstract bolded: bool with get, set
        abstract grayed: bool with get, set

    /// Node in a tree of nested declarations in a file.
    /// The top node is always a script or module node.
    type [<AllowNullLiteral>] NavigationTree =
        /// Name of the declaration, or a short description, e.g. "<class>".
        abstract text: string with get, set
        abstract kind: ScriptElementKind with get, set
        /// ScriptElementKindModifier separated by commas, e.g. "public,abstract"
        abstract kindModifiers: string with get, set
        /// Spans of the nodes that generated this declaration.
        /// There will be more than one if this is the result of merging.
        abstract spans: TextSpan[] with get, set
        abstract nameSpan: TextSpan option with get, set
        /// Present if non-empty
        abstract childItems: NavigationTree[] option with get, set

    type [<AllowNullLiteral>] CallHierarchyItem =
        abstract name: string with get, set
        abstract kind: ScriptElementKind with get, set
        abstract kindModifiers: string option with get, set
        abstract file: string with get, set
        abstract span: TextSpan with get, set
        abstract selectionSpan: TextSpan with get, set
        abstract containerName: string option with get, set

    type [<AllowNullLiteral>] CallHierarchyIncomingCall =
        abstract from: CallHierarchyItem with get, set
        abstract fromSpans: TextSpan[] with get, set

    type [<AllowNullLiteral>] CallHierarchyOutgoingCall =
        abstract ``to``: CallHierarchyItem with get, set
        abstract fromSpans: TextSpan[] with get, set

    type [<StringEnum>] [<RequireQualifiedAccess>] InlayHintKind =
        | [<CompiledName "Type">] Type
        | [<CompiledName "Parameter">] Parameter
        | [<CompiledName "Enum">] Enum

    type [<AllowNullLiteral>] InlayHint =
        abstract text: string with get, set
        abstract position: float with get, set
        abstract kind: InlayHintKind with get, set
        abstract whitespaceBefore: bool option with get, set
        abstract whitespaceAfter: bool option with get, set

    type [<AllowNullLiteral>] TodoCommentDescriptor =
        abstract text: string with get, set
        abstract priority: float with get, set

    type [<AllowNullLiteral>] TodoComment =
        abstract descriptor: TodoCommentDescriptor with get, set
        abstract message: string with get, set
        abstract position: float with get, set

    type [<AllowNullLiteral>] TextChange =
        abstract span: TextSpan with get, set
        abstract newText: string with get, set

    type [<AllowNullLiteral>] FileTextChanges =
        abstract fileName: string with get, set
        abstract textChanges: TextChange[] with get, set
        abstract isNewFile: bool option with get, set

    type [<AllowNullLiteral>] CodeAction =
        /// Description of the code action to display in the UI of the editor
        abstract description: string with get, set
        /// Text changes to apply to each file as part of the code action
        abstract changes: FileTextChanges[] with get, set
        /// <summary>
        /// If the user accepts the code fix, the editor should send the action back in a <c>applyAction</c> request.
        /// This allows the language service to have side effects (e.g. installing dependencies) upon a code fix.
        /// </summary>
        abstract commands: CodeActionCommand[] option with get, set

    type [<AllowNullLiteral>] CodeFixAction =
        inherit CodeAction
        /// Short name to identify the fix, for use by telemetry.
        abstract fixName: string with get, set
        /// If present, one may call 'getCombinedCodeFix' with this fixId.
        /// This may be omitted to indicate that the code fix can't be applied in a group.
        abstract fixId: DiagnosticMessageReportsUnnecessary option with get, set
        abstract fixAllDescription: string option with get, set

    type [<AllowNullLiteral>] CombinedCodeActions =
        abstract changes: FileTextChanges[] with get, set
        abstract commands: CodeActionCommand[] option with get, set

    type CodeActionCommand =
        InstallPackageAction

    type [<AllowNullLiteral>] InstallPackageAction =
        interface end

    /// A set of one or more available refactoring actions, grouped under a parent refactoring.
    type [<AllowNullLiteral>] ApplicableRefactorInfo =
        /// The programmatic name of the refactoring
        abstract name: string with get, set
        /// A description of this refactoring category to show to the user.
        /// If the refactoring gets inlined (see below), this text will not be visible.
        abstract description: string with get, set
        /// Inlineable refactorings can have their actions hoisted out to the top level
        /// of a context menu. Non-inlineanable refactorings should always be shown inside
        /// their parent grouping.
        ///
        /// If not specified, this value is assumed to be 'true'
        abstract inlineable: bool option with get, set
        abstract actions: RefactorActionInfo[] with get, set

    /// Represents a single refactoring action - for example, the "Extract Method..." refactor might
    /// offer several actions, each corresponding to a surround class or closure to extract into.
    type [<AllowNullLiteral>] RefactorActionInfo =
        /// The programmatic name of the refactoring action
        abstract name: string with get, set
        /// A description of this refactoring action to show to the user.
        /// If the parent refactoring is inlined away, this will be the only text shown,
        /// so this description should make sense by itself if the parent is inlineable=true
        abstract description: string with get, set
        /// A message to show to the user if the refactoring cannot be applied in
        /// the current context.
        abstract notApplicableReason: string option with get, set
        /// The hierarchical dotted name of the refactor action.
        abstract kind: string option with get, set

    /// A set of edits to make in response to a refactor action, plus an optional
    /// location where renaming should be invoked from
    type [<AllowNullLiteral>] RefactorEditInfo =
        abstract edits: FileTextChanges[] with get, set
        abstract renameFilename: string option with get, set
        abstract renameLocation: float option with get, set
        abstract commands: CodeActionCommand[] option with get, set

    type [<StringEnum>] [<RequireQualifiedAccess>] RefactorTriggerReason =
        | Implicit
        | Invoked

    type [<AllowNullLiteral>] TextInsertion =
        abstract newText: string with get, set
        /// The position in newText the caret should point to after the insertion.
        abstract caretOffset: float with get, set

    type [<AllowNullLiteral>] DocumentSpan =
        abstract textSpan: TextSpan with get, set
        abstract fileName: string with get, set
        /// If the span represents a location that was remapped (e.g. via a .d.ts.map file),
        /// then the original filename and span will be specified here
        abstract originalTextSpan: TextSpan option with get, set
        abstract originalFileName: string option with get, set
        /// If DocumentSpan.textSpan is the span for name of the declaration,
        /// then this is the span for relevant declaration
        abstract contextSpan: TextSpan option with get, set
        abstract originalContextSpan: TextSpan option with get, set

    type [<AllowNullLiteral>] RenameLocation =
        inherit DocumentSpan
        abstract prefixText: string option
        abstract suffixText: string option

    type [<AllowNullLiteral>] ReferenceEntry =
        inherit DocumentSpan
        abstract isWriteAccess: bool with get, set
        abstract isDefinition: bool with get, set
        abstract isInString: bool option with get, set

    type [<AllowNullLiteral>] ImplementationLocation =
        inherit DocumentSpan
        abstract kind: ScriptElementKind with get, set
        abstract displayParts: SymbolDisplayPart[] with get, set

    type [<StringEnum>] [<RequireQualifiedAccess>] HighlightSpanKind =
        | None
        | Definition
        | Reference
        | WrittenReference

    type [<AllowNullLiteral>] HighlightSpan =
        abstract fileName: string option with get, set
        abstract isInString: bool option with get, set
        abstract textSpan: TextSpan with get, set
        abstract contextSpan: TextSpan option with get, set
        abstract kind: HighlightSpanKind with get, set

    type [<AllowNullLiteral>] NavigateToItem =
        abstract name: string with get, set
        abstract kind: ScriptElementKind with get, set
        abstract kindModifiers: string with get, set
        abstract matchKind: NavigateToItemMatchKind with get, set
        abstract isCaseSensitive: bool with get, set
        abstract fileName: string with get, set
        abstract textSpan: TextSpan with get, set
        abstract containerName: string with get, set
        abstract containerKind: ScriptElementKind with get, set

    type [<RequireQualifiedAccess>] IndentStyle =
        | None = 0
        | Block = 1
        | Smart = 2

    type [<StringEnum>] [<RequireQualifiedAccess>] SemicolonPreference =
        | Ignore
        | Insert
        | Remove

    type [<AllowNullLiteral>] EditorOptions =
        abstract BaseIndentSize: float option with get, set
        abstract IndentSize: float with get, set
        abstract TabSize: float with get, set
        abstract NewLineCharacter: string with get, set
        abstract ConvertTabsToSpaces: bool with get, set
        abstract IndentStyle: IndentStyle with get, set

    type [<AllowNullLiteral>] EditorSettings =
        abstract baseIndentSize: float option with get, set
        abstract indentSize: float option with get, set
        abstract tabSize: float option with get, set
        abstract newLineCharacter: string option with get, set
        abstract convertTabsToSpaces: bool option with get, set
        abstract indentStyle: IndentStyle option with get, set
        abstract trimTrailingWhitespace: bool option with get, set

    type [<AllowNullLiteral>] FormatCodeOptions =
        inherit EditorOptions
        abstract InsertSpaceAfterCommaDelimiter: bool with get, set
        abstract InsertSpaceAfterSemicolonInForStatements: bool with get, set
        abstract InsertSpaceBeforeAndAfterBinaryOperators: bool with get, set
        abstract InsertSpaceAfterConstructor: bool option with get, set
        abstract InsertSpaceAfterKeywordsInControlFlowStatements: bool with get, set
        abstract InsertSpaceAfterFunctionKeywordForAnonymousFunctions: bool with get, set
        abstract InsertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis: bool with get, set
        abstract InsertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets: bool with get, set
        abstract InsertSpaceAfterOpeningAndBeforeClosingNonemptyBraces: bool option with get, set
        abstract InsertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces: bool with get, set
        abstract InsertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces: bool option with get, set
        abstract InsertSpaceAfterTypeAssertion: bool option with get, set
        abstract InsertSpaceBeforeFunctionParenthesis: bool option with get, set
        abstract PlaceOpenBraceOnNewLineForFunctions: bool with get, set
        abstract PlaceOpenBraceOnNewLineForControlBlocks: bool with get, set
        abstract insertSpaceBeforeTypeAnnotation: bool option with get, set

    type [<AllowNullLiteral>] FormatCodeSettings =
        inherit EditorSettings
        abstract insertSpaceAfterCommaDelimiter: bool option
        abstract insertSpaceAfterSemicolonInForStatements: bool option
        abstract insertSpaceBeforeAndAfterBinaryOperators: bool option
        abstract insertSpaceAfterConstructor: bool option
        abstract insertSpaceAfterKeywordsInControlFlowStatements: bool option
        abstract insertSpaceAfterFunctionKeywordForAnonymousFunctions: bool option
        abstract insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis: bool option
        abstract insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets: bool option
        abstract insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces: bool option
        abstract insertSpaceAfterOpeningAndBeforeClosingEmptyBraces: bool option
        abstract insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces: bool option
        abstract insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces: bool option
        abstract insertSpaceAfterTypeAssertion: bool option
        abstract insertSpaceBeforeFunctionParenthesis: bool option
        abstract placeOpenBraceOnNewLineForFunctions: bool option
        abstract placeOpenBraceOnNewLineForControlBlocks: bool option
        abstract insertSpaceBeforeTypeAnnotation: bool option
        abstract indentMultiLineObjectLiteralBeginningOnBlankLine: bool option
        abstract semicolons: SemicolonPreference option

    type [<AllowNullLiteral>] DefinitionInfo =
        inherit DocumentSpan
        abstract kind: ScriptElementKind with get, set
        abstract name: string with get, set
        abstract containerKind: ScriptElementKind with get, set
        abstract containerName: string with get, set
        abstract unverified: bool option with get, set

    type [<AllowNullLiteral>] DefinitionInfoAndBoundSpan =
        abstract definitions: DefinitionInfo[] option with get, set
        abstract textSpan: TextSpan with get, set

    type [<AllowNullLiteral>] ReferencedSymbolDefinitionInfo =
        inherit DefinitionInfo
        abstract displayParts: SymbolDisplayPart[] with get, set

    type [<AllowNullLiteral>] ReferencedSymbol =
        abstract definition: ReferencedSymbolDefinitionInfo with get, set
        abstract references: ReferenceEntry[] with get, set

    type [<RequireQualifiedAccess>] SymbolDisplayPartKind =
        | AliasName = 0
        | ClassName = 1
        | EnumName = 2
        | FieldName = 3
        | InterfaceName = 4
        | Keyword = 5
        | LineBreak = 6
        | NumericLiteral = 7
        | StringLiteral = 8
        | LocalName = 9
        | MethodName = 10
        | ModuleName = 11
        | Operator = 12
        | ParameterName = 13
        | PropertyName = 14
        | Punctuation = 15
        | Space = 16
        | Text = 17
        | TypeParameterName = 18
        | EnumMemberName = 19
        | FunctionName = 20
        | RegularExpressionLiteral = 21
        | Link = 22
        | LinkName = 23
        | LinkText = 24

    type [<AllowNullLiteral>] SymbolDisplayPart =
        abstract text: string with get, set
        abstract kind: string with get, set

    type [<AllowNullLiteral>] JSDocLinkDisplayPart =
        inherit SymbolDisplayPart
        abstract target: DocumentSpan with get, set

    type [<AllowNullLiteral>] JSDocTagInfo =
        abstract name: string with get, set
        abstract text: SymbolDisplayPart[] option with get, set

    type [<AllowNullLiteral>] QuickInfo =
        abstract kind: ScriptElementKind with get, set
        abstract kindModifiers: string with get, set
        abstract textSpan: TextSpan with get, set
        abstract displayParts: SymbolDisplayPart[] option with get, set
        abstract documentation: SymbolDisplayPart[] option with get, set
        abstract tags: JSDocTagInfo[] option with get, set

    type [<TypeScriptTaggedUnion("canRename")>] [<RequireQualifiedAccess>] RenameInfo =
        | [<CompiledValue false>] RenameInfoFailure of RenameInfoFailure
        | [<CompiledValue true>] RenameInfoSuccess of RenameInfoSuccess

    type [<AllowNullLiteral>] RenameInfoSuccess =
        abstract canRename: bool with get, set
        /// <summary>
        /// File or directory to rename.
        /// If set, <c>getEditsForFileRename</c> should be called instead of <c>findRenameLocations</c>.
        /// </summary>
        abstract fileToRename: string option with get, set
        abstract displayName: string with get, set
        abstract fullDisplayName: string with get, set
        abstract kind: ScriptElementKind with get, set
        abstract kindModifiers: string with get, set
        abstract triggerSpan: TextSpan with get, set

    type [<AllowNullLiteral>] RenameInfoFailure =
        abstract canRename: bool with get, set
        abstract localizedErrorMessage: string with get, set

    type [<AllowNullLiteral>] RenameInfoOptions =
        abstract allowRenameOfImportPath: bool option

    type [<AllowNullLiteral>] DocCommentTemplateOptions =
        abstract generateReturnInDocTemplate: bool option

    type [<AllowNullLiteral>] SignatureHelpParameter =
        abstract name: string with get, set
        abstract documentation: SymbolDisplayPart[] with get, set
        abstract displayParts: SymbolDisplayPart[] with get, set
        abstract isOptional: bool with get, set
        abstract isRest: bool option with get, set

    type [<AllowNullLiteral>] SelectionRange =
        abstract textSpan: TextSpan with get, set
        abstract parent: SelectionRange option with get, set

    /// Represents a single signature to show in signature help.
    /// The id is used for subsequent calls into the language service to ask questions about the
    /// signature help item in the context of any documents that have been updated.  i.e. after
    /// an edit has happened, while signature help is still active, the host can ask important
    /// questions like 'what parameter is the user currently contained within?'.
    type [<AllowNullLiteral>] SignatureHelpItem =
        abstract isVariadic: bool with get, set
        abstract prefixDisplayParts: SymbolDisplayPart[] with get, set
        abstract suffixDisplayParts: SymbolDisplayPart[] with get, set
        abstract separatorDisplayParts: SymbolDisplayPart[] with get, set
        abstract parameters: SignatureHelpParameter[] with get, set
        abstract documentation: SymbolDisplayPart[] with get, set
        abstract tags: JSDocTagInfo[] with get, set

    /// Represents a set of signature help items, and the preferred item that should be selected.
    type [<AllowNullLiteral>] SignatureHelpItems =
        abstract items: SignatureHelpItem[] with get, set
        abstract applicableSpan: TextSpan with get, set
        abstract selectedItemIndex: float with get, set
        abstract argumentIndex: float with get, set
        abstract argumentCount: float with get, set

    type [<AllowNullLiteral>] CompletionInfo =
        /// <summary>Not true for all global completions. This will be true if the enclosing scope matches a few syntax kinds. See <c>isSnippetScope</c>.</summary>
        abstract isGlobalCompletion: bool with get, set
        abstract isMemberCompletion: bool with get, set
        /// <summary>
        /// In the absence of `CompletionEntry["replacementSpan"], the editor may choose whether to use
        /// this span or its default one. If <c>CompletionEntry["replacementSpan"]</c> is defined, that span
        /// must be used to commit that completion entry.
        /// </summary>
        abstract optionalReplacementSpan: TextSpan option with get, set
        /// true when the current location also allows for a new identifier
        abstract isNewIdentifierLocation: bool with get, set
        /// Indicates to client to continue requesting completions on subsequent keystrokes.
        abstract isIncomplete: bool option with get, set
        abstract entries: CompletionEntry[] with get, set

    type [<AllowNullLiteral>] CompletionEntryDataAutoImport =
        /// The name of the property or export in the module's symbol table. Differs from the completion name
        /// in the case of InternalSymbolName.ExportEquals and InternalSymbolName.Default.
        abstract exportName: string with get, set
        abstract moduleSpecifier: string option with get, set
        /// The file name declaring the export's module symbol, if it was an external module
        abstract fileName: string option with get, set
        /// The module name (with quotes stripped) of the export's module symbol, if it was an ambient module
        abstract ambientModuleName: string option with get, set
        /// True if the export was found in the package.json AutoImportProvider
        abstract isPackageJsonImport: bool option with get, set

    type [<AllowNullLiteral>] CompletionEntryDataUnresolved =
        inherit CompletionEntryDataAutoImport
        /// <summary>The key in the <c>ExportMapCache</c> where the completion entry's <c>SymbolExportInfo[]</c> is found</summary>
        abstract exportMapKey: string with get, set

    type [<AllowNullLiteral>] CompletionEntryDataResolved =
        inherit CompletionEntryDataAutoImport
        abstract moduleSpecifier: string with get, set

    type CompletionEntryData =
        U2<CompletionEntryDataUnresolved, CompletionEntryDataResolved>

    type [<AllowNullLiteral>] CompletionEntry =
        abstract name: string with get, set
        abstract kind: ScriptElementKind with get, set
        abstract kindModifiers: string option with get, set
        abstract sortText: string with get, set
        abstract insertText: string option with get, set
        abstract isSnippet: bool option with get, set
        /// An optional span that indicates the text to be replaced by this completion item.
        /// If present, this span should be used instead of the default one.
        /// It will be set if the required span differs from the one generated by the default replacement behavior.
        abstract replacementSpan: TextSpan option with get, set
        abstract hasAction: bool option with get, set
        abstract source: string option with get, set
        abstract sourceDisplay: SymbolDisplayPart[] option with get, set
        abstract isRecommended: bool option with get, set
        abstract isFromUncheckedFile: bool option with get, set
        abstract isPackageJsonImport: bool option with get, set
        abstract isImportStatementCompletion: bool option with get, set
        /// <summary>
        /// A property to be sent back to TS Server in the CompletionDetailsRequest, along with <c>name</c>,
        /// that allows TS Server to look up the symbol represented by the completion item, disambiguating
        /// items with the same name. Currently only defined for auto-import completions, but the type is
        /// <c>unknown</c> in the protocol, so it can be changed as needed to support other kinds of completions.
        /// The presence of this property should generally not be used to assume that this completion entry
        /// is an auto-import.
        /// </summary>
        abstract data: CompletionEntryData option with get, set

    type [<AllowNullLiteral>] CompletionEntryDetails =
        abstract name: string with get, set
        abstract kind: ScriptElementKind with get, set
        abstract kindModifiers: string with get, set
        abstract displayParts: SymbolDisplayPart[] with get, set
        abstract documentation: SymbolDisplayPart[] option with get, set
        abstract tags: JSDocTagInfo[] option with get, set
        abstract codeActions: CodeAction[] option with get, set
        [<Obsolete("Use `sourceDisplay` instead.")>]
        abstract source: SymbolDisplayPart[] option with get, set
        abstract sourceDisplay: SymbolDisplayPart[] option with get, set

    type [<AllowNullLiteral>] OutliningSpan =
        /// The span of the document to actually collapse.
        abstract textSpan: TextSpan with get, set
        /// The span of the document to display when the user hovers over the collapsed span.
        abstract hintSpan: TextSpan with get, set
        /// The text to display in the editor for the collapsed region.
        abstract bannerText: string with get, set
        /// Whether or not this region should be automatically collapsed when
        /// the 'Collapse to Definitions' command is invoked.
        abstract autoCollapse: bool with get, set
        /// Classification of the contents of the span
        abstract kind: OutliningSpanKind with get, set

    type [<StringEnum>] [<RequireQualifiedAccess>] OutliningSpanKind =
        /// Single or multi-line comments
        | Comment
        /// Sections marked by '// #region' and '// #endregion' comments
        | Region
        /// Declarations and expressions
        | Code
        /// Contiguous blocks of import declarations
        | Imports

    type [<RequireQualifiedAccess>] OutputFileType =
        | JavaScript = 0
        | SourceMap = 1
        | Declaration = 2

    type [<RequireQualifiedAccess>] EndOfLineState =
        | None = 0
        | InMultiLineCommentTrivia = 1
        | InSingleQuoteStringLiteral = 2
        | InDoubleQuoteStringLiteral = 3
        | InTemplateHeadOrNoSubstitutionTemplate = 4
        | InTemplateMiddleOrTail = 5
        | InTemplateSubstitutionPosition = 6

    type [<RequireQualifiedAccess>] TokenClass =
        | Punctuation = 0
        | Keyword = 1
        | Operator = 2
        | Comment = 3
        | Whitespace = 4
        | Identifier = 5
        | NumberLiteral = 6
        | BigIntLiteral = 7
        | StringLiteral = 8
        | RegExpLiteral = 9

    type [<AllowNullLiteral>] ClassificationResult =
        abstract finalLexState: EndOfLineState with get, set
        abstract entries: ClassificationInfo[] with get, set

    type [<AllowNullLiteral>] ClassificationInfo =
        abstract length: float with get, set
        abstract classification: TokenClass with get, set

    type [<AllowNullLiteral>] Classifier =
        /// <summary>
        /// Gives lexical classifications of tokens on a line without any syntactic context.
        /// For instance, a token consisting of the text 'string' can be either an identifier
        /// named 'string' or the keyword 'string', however, because this classifier is not aware,
        /// it relies on certain heuristics to give acceptable results. For classifications where
        /// speed trumps accuracy, this function is preferable; however, for true accuracy, the
        /// syntactic classifier is ideal. In fact, in certain editing scenarios, combining the
        /// lexical, syntactic, and semantic classifiers may issue the best user experience.
        /// </summary>
        /// <param name="text">The text of a line to classify.</param>
        /// <param name="lexState">The state of the lexical classifier at the end of the previous line.</param>
        /// <param name="syntacticClassifierAbsent">
        /// Whether the client is *not* using a syntactic classifier.
        /// If there is no syntactic classifier (syntacticClassifierAbsent=true),
        /// certain heuristics may be used in its place; however, if there is a
        /// syntactic classifier (syntacticClassifierAbsent=false), certain
        /// classifications which may be incorrectly categorized will be given
        /// back as Identifiers in order to allow the syntactic classifier to
        /// subsume the classification.
        /// </param>
        [<Obsolete("Use getLexicalClassifications instead.")>]
        abstract getClassificationsForLine: text: string * lexState: EndOfLineState * syntacticClassifierAbsent: bool -> ClassificationResult
        abstract getEncodedLexicalClassifications: text: string * endOfLineState: EndOfLineState * syntacticClassifierAbsent: bool -> Classifications

    type [<StringEnum>] [<RequireQualifiedAccess>] ScriptElementKind =
        | [<CompiledName "">] Unknown
        | Warning
        /// predefined type (void) or keyword (class)
        | Keyword
        /// top level script node
        | [<CompiledName "script">] ScriptElement
        /// module foo {}
        | [<CompiledName "module">] ModuleElement
        /// class X {}
        | [<CompiledName "class">] ClassElement
        /// var x = class X {}
        | [<CompiledName "local class">] LocalClassElement
        /// interface Y {}
        | [<CompiledName "interface">] InterfaceElement
        /// type T = ...
        | [<CompiledName "type">] TypeElement
        /// enum E
        | [<CompiledName "enum">] EnumElement
        | [<CompiledName "enum member">] EnumMemberElement
        /// Inside module and script only
        /// const v = ..
        | [<CompiledName "var">] VariableElement
        /// Inside function
        | [<CompiledName "local var">] LocalVariableElement
        /// Inside module and script only
        /// function f() { }
        | [<CompiledName "function">] FunctionElement
        /// Inside function
        | [<CompiledName "local function">] LocalFunctionElement
        /// class X { [public|private]* foo() {} }
        | [<CompiledName "method">] MemberFunctionElement
        /// class X { [public|private]* [get|set] foo:number; }
        | [<CompiledName "getter">] MemberGetAccessorElement
        | [<CompiledName "setter">] MemberSetAccessorElement
        /// class X { [public|private]* foo:number; }
        /// interface Y { foo:number; }
        | [<CompiledName "property">] MemberVariableElement
        /// class X { constructor() { } }
        /// class X { static { } }
        | [<CompiledName "constructor">] ConstructorImplementationElement
        /// interface Y { ():number; }
        | [<CompiledName "call">] CallSignatureElement
        /// interface Y { []:number; }
        | [<CompiledName "index">] IndexSignatureElement
        /// interface Y { new():Y; }
        | [<CompiledName "construct">] ConstructSignatureElement
        /// function foo(*Y*: string)
        | [<CompiledName "parameter">] ParameterElement
        | [<CompiledName "type parameter">] TypeParameterElement
        | [<CompiledName "primitive type">] PrimitiveType
        | Label
        | Alias
        | [<CompiledName "const">] ConstElement
        | [<CompiledName "let">] LetElement
        | Directory
        | [<CompiledName "external module name">] ExternalModuleName
        /// <summary>&lt;JsxTagName attribute1 attribute2={0} /&gt;</summary>
        /// <deprecated />
        | [<CompiledName "JSX attribute">] JsxAttribute
        /// String literal
        | String
        /// <summary>Jsdoc @link: in <c>{@link C link text}</c>, the before and after text "<see cref=""">and "</see>"</summary>
        | Link
        /// <summary>Jsdoc @link: in <c>{@link C link text}</c>, the entity name "C"</summary>
        | [<CompiledName "link name">] LinkName
        /// <summary>Jsdoc @link: in <c>{@link C link text}</c>, the link text "link text"</summary>
        | [<CompiledName "link text">] LinkText

    type [<StringEnum>] [<RequireQualifiedAccess>] ScriptElementKindModifier =
        | [<CompiledName "">] None
        | [<CompiledName "public">] PublicMemberModifier
        | [<CompiledName "private">] PrivateMemberModifier
        | [<CompiledName "protected">] ProtectedMemberModifier
        | [<CompiledName "export">] ExportedModifier
        | [<CompiledName "declare">] AmbientModifier
        | [<CompiledName "static">] StaticModifier
        | [<CompiledName "abstract">] AbstractModifier
        | [<CompiledName "optional">] OptionalModifier
        | [<CompiledName "deprecated">] DeprecatedModifier
        | [<CompiledName ".d.ts">] DtsModifier
        | [<CompiledName ".ts">] TsModifier
        | [<CompiledName ".tsx">] TsxModifier
        | [<CompiledName ".js">] JsModifier
        | [<CompiledName ".jsx">] JsxModifier
        | [<CompiledName ".json">] JsonModifier
        | [<CompiledName ".d.mts">] DmtsModifier
        | [<CompiledName ".mts">] MtsModifier
        | [<CompiledName ".mjs">] MjsModifier
        | [<CompiledName ".d.cts">] DctsModifier
        | [<CompiledName ".cts">] CtsModifier
        | [<CompiledName ".cjs">] CjsModifier

    type [<StringEnum>] [<RequireQualifiedAccess>] ClassificationTypeNames =
        | Comment
        | Identifier
        | Keyword
        | [<CompiledName "number">] NumericLiteral
        | [<CompiledName "bigint">] BigintLiteral
        | Operator
        | [<CompiledName "string">] StringLiteral
        | [<CompiledName "whitespace">] WhiteSpace
        | Text
        | Punctuation
        | [<CompiledName "class name">] ClassName
        | [<CompiledName "enum name">] EnumName
        | [<CompiledName "interface name">] InterfaceName
        | [<CompiledName "module name">] ModuleName
        | [<CompiledName "type parameter name">] TypeParameterName
        | [<CompiledName "type alias name">] TypeAliasName
        | [<CompiledName "parameter name">] ParameterName
        | [<CompiledName "doc comment tag name">] DocCommentTagName
        | [<CompiledName "jsx open tag name">] JsxOpenTagName
        | [<CompiledName "jsx close tag name">] JsxCloseTagName
        | [<CompiledName "jsx self closing tag name">] JsxSelfClosingTagName
        | [<CompiledName "jsx attribute">] JsxAttribute
        | [<CompiledName "jsx text">] JsxText
        | [<CompiledName "jsx attribute string literal value">] JsxAttributeStringLiteralValue

    type [<RequireQualifiedAccess>] ClassificationType =
        | Comment = 1
        | Identifier = 2
        | Keyword = 3
        | NumericLiteral = 4
        | Operator = 5
        | StringLiteral = 6
        | RegularExpressionLiteral = 7
        | WhiteSpace = 8
        | Text = 9
        | Punctuation = 10
        | ClassName = 11
        | EnumName = 12
        | InterfaceName = 13
        | ModuleName = 14
        | TypeParameterName = 15
        | TypeAliasName = 16
        | ParameterName = 17
        | DocCommentTagName = 18
        | JsxOpenTagName = 19
        | JsxCloseTagName = 20
        | JsxSelfClosingTagName = 21
        | JsxAttribute = 22
        | JsxText = 23
        | JsxAttributeStringLiteralValue = 24
        | BigintLiteral = 25

    type [<AllowNullLiteral>] InlayHintsContext =
        abstract file: SourceFile with get, set
        abstract program: Program with get, set
        abstract cancellationToken: CancellationToken with get, set
        abstract host: LanguageServiceHost with get, set
        abstract span: TextSpan with get, set
        abstract preferences: InlayHintsOptions with get, set

    type [<AllowNullLiteral>] DocumentHighlights =
        abstract fileName: string with get, set
        abstract highlightSpans: HighlightSpan[] with get, set

    /// The document registry represents a store of SourceFile objects that can be shared between
    /// multiple LanguageService instances. A LanguageService instance holds on the SourceFile (AST)
    /// of files in the context.
    /// SourceFile objects account for most of the memory usage by the language service. Sharing
    /// the same DocumentRegistry instance between different instances of LanguageService allow
    /// for more efficient memory utilization since all projects will share at least the library
    /// file (lib.d.ts).
    ///
    /// A more advanced use of the document registry is to serialize sourceFile objects to disk
    /// and re-hydrate them when needed.
    ///
    /// To create a default DocumentRegistry, use createDocumentRegistry to create one, and pass it
    /// to all subsequent createLanguageService calls.
    type [<AllowNullLiteral>] DocumentRegistry =
        /// <summary>
        /// Request a stored SourceFile with a given fileName and compilationSettings.
        /// The first call to acquire will call createLanguageServiceSourceFile to generate
        /// the SourceFile if was not found in the registry.
        /// </summary>
        /// <param name="fileName">The name of the file requested</param>
        /// <param name="compilationSettings">
        /// Some compilation settings like target affects the
        /// shape of a the resulting SourceFile. This allows the DocumentRegistry to store
        /// multiple copies of the same file for different compilation settings.
        /// </param>
        /// <param name="scriptSnapshot">
        /// Text of the file. Only used if the file was not found
        /// in the registry and a new one was created.
        /// </param>
        /// <param name="version">
        /// Current version of the file. Only used if the file was not found
        /// in the registry and a new one was created.
        /// </param>
        abstract acquireDocument: fileName: string * compilationSettings: CompilerOptions * scriptSnapshot: IScriptSnapshot * version: string * ?scriptKind: ScriptKind -> SourceFile
        abstract acquireDocumentWithKey: fileName: string * path: Path * compilationSettings: CompilerOptions * key: DocumentRegistryBucketKey * scriptSnapshot: IScriptSnapshot * version: string * ?scriptKind: ScriptKind -> SourceFile
        /// <summary>
        /// Request an updated version of an already existing SourceFile with a given fileName
        /// and compilationSettings. The update will in-turn call updateLanguageServiceSourceFile
        /// to get an updated SourceFile.
        /// </summary>
        /// <param name="fileName">The name of the file requested</param>
        /// <param name="compilationSettings">
        /// Some compilation settings like target affects the
        /// shape of a the resulting SourceFile. This allows the DocumentRegistry to store
        /// multiple copies of the same file for different compilation settings.
        /// </param>
        /// <param name="scriptSnapshot">Text of the file.</param>
        /// <param name="version">Current version of the file.</param>
        abstract updateDocument: fileName: string * compilationSettings: CompilerOptions * scriptSnapshot: IScriptSnapshot * version: string * ?scriptKind: ScriptKind -> SourceFile
        abstract updateDocumentWithKey: fileName: string * path: Path * compilationSettings: CompilerOptions * key: DocumentRegistryBucketKey * scriptSnapshot: IScriptSnapshot * version: string * ?scriptKind: ScriptKind -> SourceFile
        abstract getKeyForCompilationSettings: settings: CompilerOptions -> DocumentRegistryBucketKey
        [<Obsolete("pass scriptKind for correctness")>]
        abstract releaseDocument: fileName: string * compilationSettings: CompilerOptions -> unit
        /// <summary>
        /// Informs the DocumentRegistry that a file is not needed any longer.
        ///
        /// Note: It is not allowed to call release on a SourceFile that was not acquired from
        /// this registry originally.
        /// </summary>
        /// <param name="fileName">The name of the file to be released</param>
        /// <param name="compilationSettings">The compilation settings used to acquire the file</param>
        /// <param name="scriptKind">The script kind of the file to be released</param>
        abstract releaseDocument: fileName: string * compilationSettings: CompilerOptions * scriptKind: ScriptKind -> unit
        [<Obsolete("pass scriptKind for correctness")>]
        abstract releaseDocumentWithKey: path: Path * key: DocumentRegistryBucketKey -> unit
        abstract releaseDocumentWithKey: path: Path * key: DocumentRegistryBucketKey * scriptKind: ScriptKind -> unit
        abstract reportStats: unit -> string

    type [<AllowNullLiteral>] DocumentRegistryBucketKey =
        interface end

    type [<AllowNullLiteral>] TranspileOptions =
        abstract compilerOptions: CompilerOptions option with get, set
        abstract fileName: string option with get, set
        abstract reportDiagnostics: bool option with get, set
        abstract moduleName: string option with get, set
        abstract renamedDependencies: MapLike<string> option with get, set
        abstract transformers: CustomTransformers option with get, set

    type [<AllowNullLiteral>] TranspileOutput =
        abstract outputText: string with get, set
        abstract diagnostics: Diagnostic[] option with get, set
        abstract sourceMapText: string option with get, set

    type [<StringEnum>] [<RequireQualifiedAccess>] IExportsCreateUnparsedSourceFile =
        | Js
        | Dts

    type [<AllowNullLiteral>] DiagnosticMessageReportsUnnecessary =
        interface end

    type [<StringEnum>] [<RequireQualifiedAccess>] UserPreferencesQuotePreference =
        | Auto
        | Double
        | Single

    type [<StringEnum>] [<RequireQualifiedAccess>] UserPreferencesImportModuleSpecifierPreference =
        | Shortest
        | [<CompiledName "project-relative">] ProjectRelative
        | Relative
        | [<CompiledName "non-relative">] NonRelative

    type [<StringEnum>] [<RequireQualifiedAccess>] UserPreferencesImportModuleSpecifierEnding =
        | Auto
        | Minimal
        | Index
        | Js

    type [<StringEnum>] [<RequireQualifiedAccess>] UserPreferencesIncludePackageJsonAutoImports =
        | Auto
        | On
        | Off

    type [<StringEnum>] [<RequireQualifiedAccess>] UserPreferencesJsxAttributeCompletionStyle =
        | Auto
        | Braces
        | None

    type [<StringEnum>] [<RequireQualifiedAccess>] PerformanceEventKind =
        | [<CompiledName "UpdateGraph">] UpdateGraph
        | [<CompiledName "CreatePackageJsonAutoImportProvider">] CreatePackageJsonAutoImportProvider

    type [<StringEnum>] [<RequireQualifiedAccess>] InlayHintsOptionsIncludeInlayParameterNameHints =
        | None
        | Literals
        | All

    type [<StringEnum>] [<RequireQualifiedAccess>] NavigateToItemMatchKind =
        | Exact
        | Prefix
        | Substring
        | CamelCase
