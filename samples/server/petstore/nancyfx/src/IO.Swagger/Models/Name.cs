using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// Model for testing model name same as property name
    /// </summary>
    public sealed class Name:  IEquatable<Name>
    { 
        /// <summary>
        /// _Name
        /// </summary>
        public int? _Name { get; private set; }

        /// <summary>
        /// SnakeCase
        /// </summary>
        public int? SnakeCase { get; private set; }

        /// <summary>
        /// Property
        /// </summary>
        public string Property { get; private set; }

        /// <summary>
        /// _123Number
        /// </summary>
        public int? _123Number { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Name.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Name()
        {
        }

        private Name(int? _Name, int? SnakeCase, string Property, int? _123Number)
        {
            
            this._Name = _Name;
            
            this.SnakeCase = SnakeCase;
            
            this.Property = Property;
            
            this._123Number = _123Number;
            
        }

        /// <summary>
        /// Returns builder of Name.
        /// </summary>
        /// <returns>NameBuilder</returns>
        public static NameBuilder Builder()
        {
            return new NameBuilder();
        }

        /// <summary>
        /// Returns NameBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>NameBuilder</returns>
        public NameBuilder With()
        {
            return Builder()
                ._Name(_Name)
                .SnakeCase(SnakeCase)
                .Property(Property)
                ._123Number(_123Number);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Name other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Name.
        /// </summary>
        /// <param name="left">Compared (Name</param>
        /// <param name="right">Compared (Name</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Name left, Name right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Name.
        /// </summary>
        /// <param name="left">Compared (Name</param>
        /// <param name="right">Compared (Name</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Name left, Name right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Name.
        /// </summary>
        public sealed class NameBuilder
        {
            private int? __Name;
            private int? _SnakeCase;
            private string _Property;
            private int? __123Number;

            internal NameBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for Name._Name property.
            /// </summary>
            /// <param name="value">_Name</param>
            public NameBuilder _Name(int? value)
            {
                __Name = value;
                return this;
            }

            /// <summary>
            /// Sets value for Name.SnakeCase property.
            /// </summary>
            /// <param name="value">SnakeCase</param>
            public NameBuilder SnakeCase(int? value)
            {
                _SnakeCase = value;
                return this;
            }

            /// <summary>
            /// Sets value for Name.Property property.
            /// </summary>
            /// <param name="value">Property</param>
            public NameBuilder Property(string value)
            {
                _Property = value;
                return this;
            }

            /// <summary>
            /// Sets value for Name._123Number property.
            /// </summary>
            /// <param name="value">_123Number</param>
            public NameBuilder _123Number(int? value)
            {
                __123Number = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Name.
            /// </summary>
            /// <returns>Name</returns>
            public Name Build()
            {
                Validate();
                return new Name(
                    _Name: __Name,
                    SnakeCase: _SnakeCase,
                    Property: _Property,
                    _123Number: __123Number
                );
            }

            private void Validate()
            { 
                if (__Name == null)
                {
                    throw new ArgumentException("_Name is a required property for Name and cannot be null");
                } 
            }
        }

        
    }
}
