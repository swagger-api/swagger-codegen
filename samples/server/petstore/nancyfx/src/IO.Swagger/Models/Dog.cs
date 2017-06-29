using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// Dog
    /// </summary>
    public sealed class Dog: Animal,  IEquatable<Dog>
    { 
        /// <summary>
        /// Breed
        /// </summary>
        public string Breed { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Dog.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Dog() : base(null, null)
        {
        }

        private Dog(string ClassName, string Color, string Breed) : base(ClassName, Color)
        {
            
            this.Breed = Breed;
            
        }

        /// <summary>
        /// Returns builder of Dog.
        /// </summary>
        /// <returns>DogBuilder</returns>
        public static new DogBuilder Builder()
        {
            return new DogBuilder();
        }

        /// <summary>
        /// Returns DogBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>DogBuilder</returns>
        public new DogBuilder With()
        {
            return Builder()
                .ClassName(ClassName)
                .Color(Color)
                .Breed(Breed);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Dog other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Dog.
        /// </summary>
        /// <param name="left">Compared (Dog</param>
        /// <param name="right">Compared (Dog</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Dog left, Dog right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Dog.
        /// </summary>
        /// <param name="left">Compared (Dog</param>
        /// <param name="right">Compared (Dog</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Dog left, Dog right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Dog.
        /// </summary>
        public sealed class DogBuilder
        {
            private string _ClassName;
            private string _Color;
            private string _Breed;

            internal DogBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
                _Color = "red";
            }

            /// <summary>
            /// Sets value for Dog.ClassName property.
            /// </summary>
            /// <param name="value">ClassName</param>
            public DogBuilder ClassName(string value)
            {
                _ClassName = value;
                return this;
            }

            /// <summary>
            /// Sets value for Dog.Color property.
            /// </summary>
            /// <param name="value">Color</param>
            public DogBuilder Color(string value)
            {
                _Color = value;
                return this;
            }

            /// <summary>
            /// Sets value for Dog.Breed property.
            /// </summary>
            /// <param name="value">Breed</param>
            public DogBuilder Breed(string value)
            {
                _Breed = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Dog.
            /// </summary>
            /// <returns>Dog</returns>
            public Dog Build()
            {
                Validate();
                return new Dog(
                    ClassName: _ClassName,
                    Color: _Color,
                    Breed: _Breed
                );
            }

            private void Validate()
            { 
                if (_ClassName == null)
                {
                    throw new ArgumentException("ClassName is a required property for Dog and cannot be null");
                } 
            }
        }

        
    }
}
