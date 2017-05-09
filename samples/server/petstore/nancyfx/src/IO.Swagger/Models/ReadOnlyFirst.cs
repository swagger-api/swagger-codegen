using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// ReadOnlyFirst
    /// </summary>
    public sealed class ReadOnlyFirst:  IEquatable<ReadOnlyFirst>
    { 
        /// <summary>
        /// Bar
        /// </summary>
        public string Bar { get; private set; }

        /// <summary>
        /// Baz
        /// </summary>
        public string Baz { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use ReadOnlyFirst.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public ReadOnlyFirst()
        {
        }

        private ReadOnlyFirst(string Bar, string Baz)
        {
            
            this.Bar = Bar;
            
            this.Baz = Baz;
            
        }

        /// <summary>
        /// Returns builder of ReadOnlyFirst.
        /// </summary>
        /// <returns>ReadOnlyFirstBuilder</returns>
        public static ReadOnlyFirstBuilder Builder()
        {
            return new ReadOnlyFirstBuilder();
        }

        /// <summary>
        /// Returns ReadOnlyFirstBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>ReadOnlyFirstBuilder</returns>
        public ReadOnlyFirstBuilder With()
        {
            return Builder()
                .Bar(Bar)
                .Baz(Baz);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(ReadOnlyFirst other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (ReadOnlyFirst.
        /// </summary>
        /// <param name="left">Compared (ReadOnlyFirst</param>
        /// <param name="right">Compared (ReadOnlyFirst</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (ReadOnlyFirst left, ReadOnlyFirst right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (ReadOnlyFirst.
        /// </summary>
        /// <param name="left">Compared (ReadOnlyFirst</param>
        /// <param name="right">Compared (ReadOnlyFirst</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (ReadOnlyFirst left, ReadOnlyFirst right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of ReadOnlyFirst.
        /// </summary>
        public sealed class ReadOnlyFirstBuilder
        {
            private string _Bar;
            private string _Baz;

            internal ReadOnlyFirstBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for ReadOnlyFirst.Bar property.
            /// </summary>
            /// <param name="value">Bar</param>
            public ReadOnlyFirstBuilder Bar(string value)
            {
                _Bar = value;
                return this;
            }

            /// <summary>
            /// Sets value for ReadOnlyFirst.Baz property.
            /// </summary>
            /// <param name="value">Baz</param>
            public ReadOnlyFirstBuilder Baz(string value)
            {
                _Baz = value;
                return this;
            }


            /// <summary>
            /// Builds instance of ReadOnlyFirst.
            /// </summary>
            /// <returns>ReadOnlyFirst</returns>
            public ReadOnlyFirst Build()
            {
                Validate();
                return new ReadOnlyFirst(
                    Bar: _Bar,
                    Baz: _Baz
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}
