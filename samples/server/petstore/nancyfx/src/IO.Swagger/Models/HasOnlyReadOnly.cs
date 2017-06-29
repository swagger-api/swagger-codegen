using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// HasOnlyReadOnly
    /// </summary>
    public sealed class HasOnlyReadOnly:  IEquatable<HasOnlyReadOnly>
    { 
        /// <summary>
        /// Bar
        /// </summary>
        public string Bar { get; private set; }

        /// <summary>
        /// Foo
        /// </summary>
        public string Foo { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use HasOnlyReadOnly.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public HasOnlyReadOnly()
        {
        }

        private HasOnlyReadOnly(string Bar, string Foo)
        {
            
            this.Bar = Bar;
            
            this.Foo = Foo;
            
        }

        /// <summary>
        /// Returns builder of HasOnlyReadOnly.
        /// </summary>
        /// <returns>HasOnlyReadOnlyBuilder</returns>
        public static HasOnlyReadOnlyBuilder Builder()
        {
            return new HasOnlyReadOnlyBuilder();
        }

        /// <summary>
        /// Returns HasOnlyReadOnlyBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>HasOnlyReadOnlyBuilder</returns>
        public HasOnlyReadOnlyBuilder With()
        {
            return Builder()
                .Bar(Bar)
                .Foo(Foo);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(HasOnlyReadOnly other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (HasOnlyReadOnly.
        /// </summary>
        /// <param name="left">Compared (HasOnlyReadOnly</param>
        /// <param name="right">Compared (HasOnlyReadOnly</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (HasOnlyReadOnly left, HasOnlyReadOnly right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (HasOnlyReadOnly.
        /// </summary>
        /// <param name="left">Compared (HasOnlyReadOnly</param>
        /// <param name="right">Compared (HasOnlyReadOnly</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (HasOnlyReadOnly left, HasOnlyReadOnly right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of HasOnlyReadOnly.
        /// </summary>
        public sealed class HasOnlyReadOnlyBuilder
        {
            private string _Bar;
            private string _Foo;

            internal HasOnlyReadOnlyBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for HasOnlyReadOnly.Bar property.
            /// </summary>
            /// <param name="value">Bar</param>
            public HasOnlyReadOnlyBuilder Bar(string value)
            {
                _Bar = value;
                return this;
            }

            /// <summary>
            /// Sets value for HasOnlyReadOnly.Foo property.
            /// </summary>
            /// <param name="value">Foo</param>
            public HasOnlyReadOnlyBuilder Foo(string value)
            {
                _Foo = value;
                return this;
            }


            /// <summary>
            /// Builds instance of HasOnlyReadOnly.
            /// </summary>
            /// <returns>HasOnlyReadOnly</returns>
            public HasOnlyReadOnly Build()
            {
                Validate();
                return new HasOnlyReadOnly(
                    Bar: _Bar,
                    Foo: _Foo
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}
