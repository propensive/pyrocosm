                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃    Pyrocosm, version 0.1.0.                                                                      ┃
┃    © Copyright 2026 Jon Pretty, Propensive OÜ.                                                   ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://propensive.dev/pyrocosm/                                                          ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package pyrocosm

// Excluded from the umbrella: `Figure` (savagery), which would
// outrank this package's own definitions, since a wildcard import beats a package member
// declared in another file.
import soundness.{Figure as _, *}

import scala.compiletime

import monotonous.alphabets.base64Standard

// The exhibitions of rich media, which exist only on the JVM because hallucination's codecs
// do: a raster image becomes a `Block.Image` whose source is a data: URI of the image in its
// own format, and an SVG drawing becomes a `Block.Figure` carrying its markup. A web frontend
// shows both as they are; a terminal draws the raster in colour and describes the drawing.
//
// Their subjects and the typeclass both live elsewhere, so the givens cannot sit in a
// companion; they are named, at package level, for import by name.

// An instance for any raster type, since `is` is exact in `Self` and a built raster is typed by
// its layout (`Raster by Rgb`), a decoded one by its format (`Raster in Png`), and either may
// carry both. The format is read from the type, one case per format hallucination implements
// (a `summonFrom` cannot bind the format as a type variable), and a raster of no format
// encodes as PNG, which is lossless and carries alpha. A lambda rather than an anonymous
// class, as `Presentable.quantity` is, since this is inline.
inline given rasterPresentable: [raster <: Raster] => raster is Presentable in Block =
  compiletime.summonFrom:
    case given (`raster` <:< (Raster in Jpeg)) => Presentable.flow[raster] { raster => Media.image(raster, Jpeg()) }
    case given (`raster` <:< (Raster in Gif))  => Presentable.flow[raster] { raster => Media.image(raster, Gif()) }
    case given (`raster` <:< (Raster in Webp)) => Presentable.flow[raster] { raster => Media.image(raster, Webp()) }
    case given (`raster` <:< (Raster in Bmp))  => Presentable.flow[raster] { raster => Media.image(raster, Bmp()) }
    case _                                     => Presentable.flow[raster] { raster => Media.image(raster, Png()) }

given svgPresentable: Svg is Presentable in Block = svg =>
  val size: Text = t"${Media.dimension(svg.width)}×${Media.dimension(svg.height)}"
  Block.Figure(Figure(Inline.text(t"$size SVG"), svg.xml.show))

object Media:
  def image(raster: Raster, rasterizable: Rasterizable): Block =
    Block.Image(dataUri(raster, rasterizable), caption(raster, rasterizable))

  def dataUri(raster: Raster, rasterizable: Rasterizable): Text =
    t"data:${rasterizable.mediaType.basic};base64,${rasterizable.encode(raster).serialize[Base64]}"

  // The description standing in for the image where it cannot be shown: its size and format.
  def caption(raster: Raster, rasterizable: Rasterizable): Text =
    t"${raster.width}×${raster.height} ${rasterizable.name}"

  // A whole number prints without its fraction.
  def dimension(value: Float): Text =
    if value == value.toInt then value.toInt.toString.tt else value.toString.tt
