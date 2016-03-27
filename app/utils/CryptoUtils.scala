package utils

import java.nio.ByteBuffer
import java.security.MessageDigest

import play.api.libs.{Codecs, Crypto}

/**
 * @author jun.
 */
object CryptoUtils {

  /**
   * Get the string representation of a random UUID.
   * See java.util.UUID.randomUUID
   * @return A string representing the random UUID
   */
  def randomUUID: String = {
    java.util.UUID.randomUUID().toString
  }

  /**
   * Get a SHA-1 hashed random UUID
   * @return A hex string of a SHA-1 hash of a java random UUID
   */
  def hashedUUID: String = {
    sha1(randomUUID.getBytes)
  }

  /**
   * SHA-1 hash of the given bytes
   * @param bytes an array of bytes needs to be hashed
   * @return A 40-character hex string of the SHA-1 result.
   */
  def sha1(bytes: Array[Byte]): String = {
    Codecs.sha1(bytes)
  }

  /**
   * A wrapper of play.api.libs.Crypto.encryptAES
   * @param data the string data that needs to be encrypted
   * @return A hex encrypted string
   */
  def AESEncrypt(data: String): String = {
    Crypto.encryptAES(data)
  }

  def AESDecrypt(data: String): String = {
    Crypto.decryptAES(data)
  }

  def randomToken: String = {
    Crypto.generateToken
  }

  def sign(data: String): Array[Byte] = {
    Codecs.hexStringToByte(Crypto.sign(data))
  }

  def sign(data: Array[Byte]): Array[Byte] = {
    Codecs.hexStringToByte(Crypto.sign(Codecs.toHexString(data)))
  }

  def base64Encode(data: Array[Byte]): String = {
    java.util.Base64.getEncoder.encodeToString(data)
  }

  def base64Decode(input: String): Option[Array[Byte]] = {
    try {
      Some(java.util.Base64.getDecoder.decode(input))
    } catch {
      case _ => None
    }
  }
}
