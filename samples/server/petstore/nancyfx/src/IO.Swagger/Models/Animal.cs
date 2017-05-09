using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// Animal
    /// </summary>
    public class Animal:  IEquatable<Animal>
    { 
        /// <summary>
        /// ClassName
        /// </summary>
        public string ClassName { get; private set; }

        /// <summary>
        /// Color
        /// </summary>
        public string Color { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Animal.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Animal()
        {
        }

        protected Animal(string ClassName, string Color)
        {
            
            this.ClassName = ClassName;
            
            this.Color = Color;
            
        }

        /// <summary>
        /// Returns builder of Animal.
        /// </summary>
        /// <returns>AnimalBuilder</returns>
        public static AnimalBuilder Builder()
        {
            return new AnimalBuilder();
        }

        /// <summary>
        /// Returns AnimalBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>AnimalBuilder</returns>
        public AnimalBuilder With()
        {
            return Builder()
                .ClassName(ClassName)
                .Color(Color);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Animal other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Animal.
        /// </summary>
        /// <param name="left">Compared (Animal</param>
        /// <param name="right">Compared (Animal</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Animal left, Animal right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Animal.
        /// </summary>
        /// <param name="left">Compared (Animal</param>
        /// <param name="right">Compared (Animal</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Animal left, Animal right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Animal.
        /// </summary>
        public sealed class AnimalBuilder
        {
            private string _ClassName;
            private string _Color;

            internal AnimalBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
                _Color = "red";
            }

            /// <summary>
            /// Sets value for Animal.ClassName property.
            /// </summary>
            /// <param name="value">ClassName</param>
            public AnimalBuilder ClassName(string value)
            {
                _ClassName = value;
                return this;
            }

            /// <summary>
            /// Sets value for Animal.Color property.
            /// </summary>
            /// <param name="value">Color</param>
            public AnimalBuilder Color(string value)
            {
                _Color = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Animal.
            /// </summary>
            /// <returns>Animal</returns>
            public Animal Build()
            {
                Validate();
                return new Animal(
                    ClassName: _ClassName,
                    Color: _Color
                );
            }

            private void Validate()
            { 
                if (_ClassName == null)
                {
                    throw new ArgumentException("ClassName is a required property for Animal and cannot be null");
                } 
            }
        }

        
    }
}
