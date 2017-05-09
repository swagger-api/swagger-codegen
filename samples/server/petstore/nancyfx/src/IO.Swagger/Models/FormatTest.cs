using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// FormatTest
    /// </summary>
    public sealed class FormatTest:  IEquatable<FormatTest>
    { 
        /// <summary>
        /// Integer
        /// </summary>
        public int? Integer { get; private set; }

        /// <summary>
        /// Int32
        /// </summary>
        public int? Int32 { get; private set; }

        /// <summary>
        /// Int64
        /// </summary>
        public long? Int64 { get; private set; }

        /// <summary>
        /// Number
        /// </summary>
        public decimal? Number { get; private set; }

        /// <summary>
        /// Float
        /// </summary>
        public float? Float { get; private set; }

        /// <summary>
        /// Double
        /// </summary>
        public double? Double { get; private set; }

        /// <summary>
        /// String
        /// </summary>
        public string String { get; private set; }

        /// <summary>
        /// Byte
        /// </summary>
        public byte[] Byte { get; private set; }

        /// <summary>
        /// Binary
        /// </summary>
        public byte[] Binary { get; private set; }

        /// <summary>
        /// Date
        /// </summary>
        public ZonedDateTime? Date { get; private set; }

        /// <summary>
        /// DateTime
        /// </summary>
        public ZonedDateTime? DateTime { get; private set; }

        /// <summary>
        /// Uuid
        /// </summary>
        public Guid? Uuid { get; private set; }

        /// <summary>
        /// Password
        /// </summary>
        public string Password { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use FormatTest.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public FormatTest()
        {
        }

        private FormatTest(int? Integer, int? Int32, long? Int64, decimal? Number, float? Float, double? Double, string String, byte[] Byte, byte[] Binary, ZonedDateTime? Date, ZonedDateTime? DateTime, Guid? Uuid, string Password)
        {
            
            this.Integer = Integer;
            
            this.Int32 = Int32;
            
            this.Int64 = Int64;
            
            this.Number = Number;
            
            this.Float = Float;
            
            this.Double = Double;
            
            this.String = String;
            
            this.Byte = Byte;
            
            this.Binary = Binary;
            
            this.Date = Date;
            
            this.DateTime = DateTime;
            
            this.Uuid = Uuid;
            
            this.Password = Password;
            
        }

        /// <summary>
        /// Returns builder of FormatTest.
        /// </summary>
        /// <returns>FormatTestBuilder</returns>
        public static FormatTestBuilder Builder()
        {
            return new FormatTestBuilder();
        }

        /// <summary>
        /// Returns FormatTestBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>FormatTestBuilder</returns>
        public FormatTestBuilder With()
        {
            return Builder()
                .Integer(Integer)
                .Int32(Int32)
                .Int64(Int64)
                .Number(Number)
                .Float(Float)
                .Double(Double)
                .String(String)
                .Byte(Byte)
                .Binary(Binary)
                .Date(Date)
                .DateTime(DateTime)
                .Uuid(Uuid)
                .Password(Password);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(FormatTest other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (FormatTest.
        /// </summary>
        /// <param name="left">Compared (FormatTest</param>
        /// <param name="right">Compared (FormatTest</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (FormatTest left, FormatTest right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (FormatTest.
        /// </summary>
        /// <param name="left">Compared (FormatTest</param>
        /// <param name="right">Compared (FormatTest</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (FormatTest left, FormatTest right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of FormatTest.
        /// </summary>
        public sealed class FormatTestBuilder
        {
            private int? _Integer;
            private int? _Int32;
            private long? _Int64;
            private decimal? _Number;
            private float? _Float;
            private double? _Double;
            private string _String;
            private byte[] _Byte;
            private byte[] _Binary;
            private ZonedDateTime? _Date;
            private ZonedDateTime? _DateTime;
            private Guid? _Uuid;
            private string _Password;

            internal FormatTestBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for FormatTest.Integer property.
            /// </summary>
            /// <param name="value">Integer</param>
            public FormatTestBuilder Integer(int? value)
            {
                _Integer = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.Int32 property.
            /// </summary>
            /// <param name="value">Int32</param>
            public FormatTestBuilder Int32(int? value)
            {
                _Int32 = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.Int64 property.
            /// </summary>
            /// <param name="value">Int64</param>
            public FormatTestBuilder Int64(long? value)
            {
                _Int64 = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.Number property.
            /// </summary>
            /// <param name="value">Number</param>
            public FormatTestBuilder Number(decimal? value)
            {
                _Number = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.Float property.
            /// </summary>
            /// <param name="value">Float</param>
            public FormatTestBuilder Float(float? value)
            {
                _Float = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.Double property.
            /// </summary>
            /// <param name="value">Double</param>
            public FormatTestBuilder Double(double? value)
            {
                _Double = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.String property.
            /// </summary>
            /// <param name="value">String</param>
            public FormatTestBuilder String(string value)
            {
                _String = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.Byte property.
            /// </summary>
            /// <param name="value">Byte</param>
            public FormatTestBuilder Byte(byte[] value)
            {
                _Byte = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.Binary property.
            /// </summary>
            /// <param name="value">Binary</param>
            public FormatTestBuilder Binary(byte[] value)
            {
                _Binary = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.Date property.
            /// </summary>
            /// <param name="value">Date</param>
            public FormatTestBuilder Date(ZonedDateTime? value)
            {
                _Date = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.DateTime property.
            /// </summary>
            /// <param name="value">DateTime</param>
            public FormatTestBuilder DateTime(ZonedDateTime? value)
            {
                _DateTime = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.Uuid property.
            /// </summary>
            /// <param name="value">Uuid</param>
            public FormatTestBuilder Uuid(Guid? value)
            {
                _Uuid = value;
                return this;
            }

            /// <summary>
            /// Sets value for FormatTest.Password property.
            /// </summary>
            /// <param name="value">Password</param>
            public FormatTestBuilder Password(string value)
            {
                _Password = value;
                return this;
            }


            /// <summary>
            /// Builds instance of FormatTest.
            /// </summary>
            /// <returns>FormatTest</returns>
            public FormatTest Build()
            {
                Validate();
                return new FormatTest(
                    Integer: _Integer,
                    Int32: _Int32,
                    Int64: _Int64,
                    Number: _Number,
                    Float: _Float,
                    Double: _Double,
                    String: _String,
                    Byte: _Byte,
                    Binary: _Binary,
                    Date: _Date,
                    DateTime: _DateTime,
                    Uuid: _Uuid,
                    Password: _Password
                );
            }

            private void Validate()
            { 
                if (_Number == null)
                {
                    throw new ArgumentException("Number is a required property for FormatTest and cannot be null");
                } 
                if (_Byte == null)
                {
                    throw new ArgumentException("Byte is a required property for FormatTest and cannot be null");
                } 
                if (_Date == null)
                {
                    throw new ArgumentException("Date is a required property for FormatTest and cannot be null");
                } 
                if (_Password == null)
                {
                    throw new ArgumentException("Password is a required property for FormatTest and cannot be null");
                } 
            }
        }

        
    }
}
