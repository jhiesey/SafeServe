module SafeBlaze (
    -- * Important types.
    Html
  , Tag
  , Attribute
  , AttributeValue

    -- * Creating attributes.
  , dataAttribute
  , customAttribute

    -- * Converting values to HTML.
  , ToHtml (..)
  , text
  , preEscapedText
  , lazyText
  , preEscapedLazyText
  , string
  , preEscapedString

    -- * Creating tags.
  , textTag
  , stringTag

    -- * Converting values to attribute values.
  , ToValue (..)
  , textValue
  , preEscapedTextValue
  , lazyTextValue
  , preEscapedLazyTextValue
  , stringValue
  , preEscapedStringValue

    -- * Setting attributes
  , (!)
  ) where
    
import Text.Blaze