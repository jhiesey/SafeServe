module SafeBlaze.Internal (
    ChoiceString (..)
  , StaticString (..)
  , HtmlM (..)
  , Html
  , Tag
  , Attribute
  , AttributeValue

    -- * Creating custom tags and attributes.
  , attribute
  , dataAttribute
  , customAttribute

    -- * Converting values to HTML.
  , text
  , preEscapedText
  , lazyText
  , preEscapedLazyText
  , string
  , preEscapedString

    -- * Converting values to tags.
  , textTag
  , stringTag

    -- * Converting values to attribute values.
  , textValue
  , preEscapedTextValue
  , lazyTextValue
  , preEscapedLazyTextValue
  , stringValue
  , preEscapedStringValue

    -- * Setting attributes
  , Attributable
  , (!)

    -- * Modifying HTML elements
  , external
  ) where
    
import Text.Blaze.Internal