using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// Cat
    /// </summary>
    public sealed class Cat: Animal,  IEquatable<Cat>
    { 
        /// <summary>
        /// Declawed
        /// </summary>
        public bool? Declawed { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Cat.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Cat() : base(null, null)
        {
        }

        private Cat(string ClassName, string Color, bool? Declawed) : base(ClassName, Color)
        {
            
            this.Declawed = Declawed;
            
        }

        /// <summary>
        /// Returns builder of Cat.
        /// </summary>
        /// <returns>CatBuilder</returns>
        public static new CatBuilder Builder()
        {
            return new CatBuilder();
        }

        /// <summary>
        /// Returns CatBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>CatBuilder</returns>
        public new CatBuilder With()
        {
            return Builder()
                .ClassName(ClassName)
                .Color(Color)
                .Declawed(Declawed);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Cat other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Cat.
        /// </summary>
        /// <param name="left">Compared (Cat</param>
        /// <param name="right">Compared (Cat</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Cat left, Cat right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Cat.
        /// </summary>
        /// <param name="left">Compared (Cat</param>
        /// <param name="right">Compared (Cat</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Cat left, Cat right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Cat.
        /// </summary>
        public sealed class CatBuilder
        {
            private string _ClassName;
            private string _Color;
            private bool? _Declawed;

            internal CatBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
                _Color = "red";
            }

            /// <summary>
            /// Sets value for Cat.ClassName property.
            /// </summary>
            /// <param name="value">ClassName</param>
            public CatBuilder ClassName(string value)
            {
                _ClassName = value;
                return this;
            }

            /// <summary>
            /// Sets value for Cat.Color property.
            /// </summary>
            /// <param name="value">Color</param>
            public CatBuilder Color(string value)
            {
                _Color = value;
                return this;
            }

            /// <summary>
            /// Sets value for Cat.Declawed property.
            /// </summary>
            /// <param name="value">Declawed</param>
            public CatBuilder Declawed(bool? value)
            {
                _Declawed = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Cat.
            /// </summary>
            /// <returns>Cat</returns>
            public Cat Build()
            {
                Validate();
                return new Cat(
                    ClassName: _ClassName,
                    Color: _Color,
                    Declawed: _Declawed
                );
            }

            private void Validate()
            { 
                if (_ClassName == null)
                {
                    throw new ArgumentException("ClassName is a required property for Cat and cannot be null");
                } 
            }
        }

        
    }
}
