module Web.HTML
  ( window
  , module Exports
  ) where

import Effect (Effect)
import Web.HTML.Common (AttrName, ClassName, PropName) as Exports
import Web.HTML.HTMLAnchorElement (HTMLAnchorElement) as Exports
import Web.HTML.HTMLAreaElement (HTMLAreaElement) as Exports
import Web.HTML.HTMLAudioElement (HTMLAudioElement) as Exports
import Web.HTML.HTMLBRElement (HTMLBRElement) as Exports
import Web.HTML.HTMLBaseElement (HTMLBaseElement) as Exports
import Web.HTML.HTMLBodyElement (HTMLBodyElement) as Exports
import Web.HTML.HTMLButtonElement (HTMLButtonElement) as Exports
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement) as Exports
import Web.HTML.HTMLDListElement (HTMLDListElement) as Exports
import Web.HTML.HTMLDataElement (HTMLDataElement) as Exports
import Web.HTML.HTMLDataListElement (HTMLDataListElement) as Exports
import Web.HTML.HTMLDivElement (HTMLDivElement) as Exports
import Web.HTML.HTMLDocument (HTMLDocument) as Exports
import Web.HTML.HTMLElement (HTMLElement) as Exports
import Web.HTML.HTMLEmbedElement (HTMLEmbedElement) as Exports
import Web.HTML.HTMLFieldSetElement (HTMLFieldSetElement) as Exports
import Web.HTML.HTMLFormElement (HTMLFormElement) as Exports
import Web.HTML.HTMLHRElement (HTMLHRElement) as Exports
import Web.HTML.HTMLHeadElement (HTMLHeadElement) as Exports
import Web.HTML.HTMLHeadingElement (HTMLHeadingElement) as Exports
import Web.HTML.HTMLIFrameElement (HTMLIFrameElement) as Exports
import Web.HTML.HTMLImageElement (HTMLImageElement) as Exports
import Web.HTML.HTMLInputElement (HTMLInputElement) as Exports
import Web.HTML.HTMLKeygenElement (HTMLKeygenElement) as Exports
import Web.HTML.HTMLLIElement (HTMLLIElement) as Exports
import Web.HTML.HTMLLabelElement (HTMLLabelElement) as Exports
import Web.HTML.HTMLLegendElement (HTMLLegendElement) as Exports
import Web.HTML.HTMLLinkElement (HTMLLinkElement) as Exports
import Web.HTML.HTMLMapElement (HTMLMapElement) as Exports
import Web.HTML.HTMLMediaElement (HTMLMediaElement) as Exports
import Web.HTML.HTMLMetaElement (HTMLMetaElement) as Exports
import Web.HTML.HTMLMeterElement (HTMLMeterElement) as Exports
import Web.HTML.HTMLModElement (HTMLModElement) as Exports
import Web.HTML.HTMLOListElement (HTMLOListElement) as Exports
import Web.HTML.HTMLObjectElement (HTMLObjectElement) as Exports
import Web.HTML.HTMLOptGroupElement (HTMLOptGroupElement) as Exports
import Web.HTML.HTMLOptionElement (HTMLOptionElement) as Exports
import Web.HTML.HTMLOutputElement (HTMLOutputElement) as Exports
import Web.HTML.HTMLParagraphElement (HTMLParagraphElement) as Exports
import Web.HTML.HTMLParamElement (HTMLParamElement) as Exports
import Web.HTML.HTMLPreElement (HTMLPreElement) as Exports
import Web.HTML.HTMLProgressElement (HTMLProgressElement) as Exports
import Web.HTML.HTMLQuoteElement (HTMLQuoteElement) as Exports
import Web.HTML.HTMLScriptElement (HTMLScriptElement) as Exports
import Web.HTML.HTMLSelectElement (HTMLSelectElement) as Exports
import Web.HTML.HTMLSourceElement (HTMLSourceElement) as Exports
import Web.HTML.HTMLSpanElement (HTMLSpanElement) as Exports
import Web.HTML.HTMLStyleElement (HTMLStyleElement) as Exports
import Web.HTML.HTMLTableCaptionElement (HTMLTableCaptionElement) as Exports
import Web.HTML.HTMLTableCellElement (HTMLTableCellElement) as Exports
import Web.HTML.HTMLTableColElement (HTMLTableColElement) as Exports
import Web.HTML.HTMLTableDataCellElement (HTMLTableDataCellElement) as Exports
import Web.HTML.HTMLTableElement (HTMLTableElement) as Exports
import Web.HTML.HTMLTableHeaderCellElement (HTMLTableHeaderCellElement) as Exports
import Web.HTML.HTMLTableRowElement (HTMLTableRowElement) as Exports
import Web.HTML.HTMLTableSectionElement (HTMLTableSectionElement) as Exports
import Web.HTML.HTMLTemplateElement (HTMLTemplateElement) as Exports
import Web.HTML.HTMLTextAreaElement (HTMLTextAreaElement) as Exports
import Web.HTML.HTMLTimeElement (HTMLTimeElement) as Exports
import Web.HTML.HTMLTitleElement (HTMLTitleElement) as Exports
import Web.HTML.HTMLTrackElement (HTMLTrackElement) as Exports
import Web.HTML.HTMLUListElement (HTMLUListElement) as Exports
import Web.HTML.HTMLVideoElement (HTMLVideoElement) as Exports
import Web.HTML.History (History) as Exports
import Web.HTML.Location (Location) as Exports
import Web.HTML.Navigator (Navigator) as Exports
import Web.HTML.Window (Window)
import Web.HTML.Window (Window) as Exports

foreign import window :: Effect Window
